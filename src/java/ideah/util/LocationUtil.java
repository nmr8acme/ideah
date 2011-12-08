package ideah.util;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.lang.reflect.Array;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

public final class LocationUtil {

    private static final Logger LOG = Logger.getInstance("ideah.util.LocationUtil");

    private static Long sourcesLastModified = null;

    static String getExeName(String file) {
        return SystemInfo.isWindows
                ? file + ".exe"
                : file;
    }

    static String getGhcCommandPath(VirtualFile ghcHome) {
        if (ghcHome == null)
            return null;
        VirtualFile virBin = ghcHome.findChild("bin");
        if (virBin == null)
            return null;
        return new File(virBin.getPath(), "ghc").getAbsolutePath();
    }

    interface HsCallback {

        void run(ZipInputStream zis, ZipEntry entry) throws IOException;
    }

    static void listHaskellSources(HsCallback callback) throws IOException {
        InputStream is = CompilerLocation.class.getResourceAsStream("/ask_ghc.jar");
        ZipInputStream zis = new ZipInputStream(is);
        while (true) {
            ZipEntry entry = zis.getNextEntry();
            if (entry == null)
                break;
            callback.run(zis, entry);
            zis.closeEntry();
        }
        zis.close();
    }

    public static String rootsAsString(Module module, boolean tests) {
        VirtualFile[] sourceRoots = ModuleRootManager.getInstance(module).getSourceRoots(tests);
        StringBuilder buf = new StringBuilder();
        for (VirtualFile root : sourceRoots) {
            buf.append(':').append(root.getPath());
        }
        return buf.substring(1);
    }

    private static void cabalInstall(@Nullable ProgressIndicator indicator, @Nullable String cabalPath, @Nullable List<String> args) {
        if (args == null || args.isEmpty())
            return;
        if (indicator != null) {
            StringBuilder builder = new StringBuilder("Installing missing package").append(
                args.size() > 1
                    ? "s ("
                    : " (");
            String comma = ", ";
            for (String pkg : args) {
                builder.append(pkg).append(comma);
            }
            int lastComma = builder.lastIndexOf(comma);
            String dots = ")...";
            if (lastComma > 0) {
                builder.replace(lastComma, lastComma + comma.length(), dots);
            } else {
                builder.append(dots);
            }
            indicator.setText(builder.toString());
        }
        List<String> cabalArgsList = new ArrayList<String>();
        if (cabalPath == null)
            return; // todo: force user to specify cabal installation path
        try {
            runCabal(cabalPath, Arrays.asList("update"));
            String installStr = "install";
            cabalArgsList.add(installStr);
            if (indicator == null) {
                cabalArgsList.addAll(args);
                runCabal(cabalPath, cabalArgsList);
            } else {
                double progress = 1.0 / args.size();
                for (String arg : args) {
                    runCabal(cabalPath, Arrays.asList(installStr, arg));
                    indicator.setFraction(indicator.getFraction() + progress);
                }
            }
        } catch (Exception e) {
            LOG.error(e);
        }
    }

    private static void runCabal(String cabalPath, List<String> cabalArgsList) throws IOException, InterruptedException {
        Process cabalProcess = Runtime.getRuntime().exec(cabalPath, cabalArgsList.toArray(new String[cabalArgsList.size()]));
        cabalProcess.waitFor();
    }

    // todo: HTTP proxy settings
    static void cabalCheckAndInstall(@Nullable ProgressIndicator indicator, String... packages) {
        if (packages.length > 0) {
            String cabalPath = getPathFor("cabal");
            List<String> missingPackages = getMissingPackages(cabalPath, Arrays.asList(packages));
            try {
                cabalInstall(indicator, cabalPath, missingPackages);
            } catch (Exception e) {
                LOG.error(e);
            }
        }
    }

    private static String getPathFor(String name) {
        String path = System.getenv("PATH");
        String exeName = getExeName(name);
        StringTokenizer stringTokenizer = new StringTokenizer(path, File.pathSeparator);
        while (stringTokenizer.hasMoreElements()) {
            String dir = stringTokenizer.nextToken();
            File directory = new File(dir);
            if (directory.isDirectory()) {
                File file = new File(directory, exeName);
                if (file.exists())
                    return new File(directory, name).getAbsolutePath();
            }
        }
        return null;
    }

    private static List<String> getMissingPackages(String cabalPath, List<String> packages) {
        List<String> missingPackages = null;
        try {
            if (cabalPath == null)
                return packages; // todo: produce user error
            List<String> args = new ArrayList<String>();
            args.addAll(Arrays.asList(cabalPath, "list", "--installed", "-v0", "--simple-output"));
            Map<String, String> argsPackages = new HashMap<String, String>();
            for (String pkg : packages) {
                argsPackages.put(pkg, pkg.substring(0, pkg.lastIndexOf('-')));
            }
            args.addAll(argsPackages.values()); // For "haddock list --installed <package name>", the package name should not include the version
            Collections.sort(packages);
            missingPackages = new ArrayList<String>();
            Iterator<String> iterator = packages.iterator();
            ProcessLauncher getMissingPackagesLauncher = new ProcessLauncher(true, null, args);
            BufferedReader reader = new BufferedReader(new StringReader(getMissingPackagesLauncher.getStdOut()));
            String line = reader.readLine();
            while (line != null && iterator.hasNext()) {
                String next = iterator.next();
                if (!(line.startsWith(argsPackages.get(next)) && equalVersion(getVersion(next), getVersion(line)))) {
                    missingPackages.add(next);
                } else {
                    line = reader.readLine();
                }
            }
        } catch (Exception e) {
            LOG.error(e);
        }
        return missingPackages;
    }

    public static ArrayList<Integer> getVersion(@NotNull String name) {
        String[] versionStr = name.split("[^0-9]");
        int length = versionStr.length;
        ArrayList<Integer> version = new ArrayList<Integer>();
        for (int i = 0; i < length; i++) {
            try {
                version.add(Integer.parseInt(versionStr[i]));
            } catch (NumberFormatException ignored) {
                break;
            }
        }
        return version;
    }

    public static boolean equalVersion(@NotNull ArrayList<Integer> v1, @NotNull ArrayList<Integer> v2) {
        int size = v1.size();
        if (size != v2.size())
            return false;
        for (int i = 0; i < size; i++) {
            if (!v1.get(i).equals(v2.get(i)))
                return false;
        }
        return true;
    }

    static boolean needRecompile(File compilerExe) throws IOException {
           if (compilerExe.exists()) {
               if (sourcesLastModified == null) {
                   final Long[] maxModified = new Long[1];
                   listHaskellSources(new HsCallback() {
                       public void run(ZipInputStream zis, ZipEntry entry) {
                           long lastModified = entry.getTime();
                           if (maxModified[0] == null) {
                               maxModified[0] = lastModified;
                           } else {
                               maxModified[0] = Math.max(maxModified[0].longValue(), lastModified);
                           }
                       }
                   });
                   sourcesLastModified = maxModified[0];
               }
               return sourcesLastModified != null && sourcesLastModified.longValue() > compilerExe.lastModified();
           } else {
               return true;
           }
       }

}