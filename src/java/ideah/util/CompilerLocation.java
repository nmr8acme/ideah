package ideah.util;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.StreamUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.StatusBar;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

public final class CompilerLocation {

    private static final Logger LOG = Logger.getInstance("ideah.util.CompilerLocation");
    private static final String MAIN_FILE = "ask_ghc";

    public final String exe;
    public final String libPath;

    private CompilerLocation(String exe, String libPath) {
        this.exe = exe;
        this.libPath = libPath;
    }

    public static synchronized CompilerLocation get(@Nullable Module module) {
        if (module == null)
            return null;
        Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
        if (sdk == null)
            return null;
        VirtualFile ghcHome = sdk.getHomeDirectory();
        if (ghcHome == null)
            return null;
        String ghcLib = null;
        try {
            // todo: cache result somewhere (or better store in SDK settings)
            String ghcCommandPath = LocationUtil.getGhcCommandPath(ghcHome);
            if (ghcCommandPath == null)
                return null;
            ProcessLauncher getLibdirLauncher = new ProcessLauncher(true, null, ghcCommandPath, "--print-libdir");
            ghcLib = getLibdirLauncher.getStdOut();
        } catch (Exception e) {
            LOG.error(e);
        }
        if (ghcLib == null)
            return null;
        ghcLib = ghcLib.trim();
        try {
            File pluginPath = new File(new File(System.getProperty("user.home"), ".ideah"), sdk.getVersionString());
            pluginPath.mkdirs();
            File compilerExe = new File(pluginPath, LocationUtil.getExeName(MAIN_FILE));
            if (LocationUtil.needRecompile(compilerExe)) {
                if (!compileHs(module, pluginPath, ghcHome, compilerExe))
                    return null;
            }
            if (compilerExe.exists())
                return new CompilerLocation(compilerExe.getAbsolutePath(), ghcLib);
            else
                return null;
        } catch (Exception ex) {
            LOG.error(ex);
            return null;
        }
    }

    @Nullable
    public static String suggestLibPath(@Nullable Sdk sdk) {
        if (sdk == null)
            return null;
        VirtualFile ghcHome = sdk.getHomeDirectory();
        if (ghcHome == null)
            return null;
        String ghcLib = null;
        try {
            String ghcCommandPath = LocationUtil.getGhcCommandPath(ghcHome);
            if (ghcCommandPath == null)
                return null;
            ProcessLauncher getLibdirLauncher = new ProcessLauncher(true, null, ghcCommandPath, "--print-libdir");
            ghcLib = getLibdirLauncher.getStdOut().trim();
        } catch (Exception e) {
            LOG.error(e);
        }
        return ghcLib;
    }

    @Nullable
    public static String suggestCabalPath(@Nullable Sdk sdk) {
        String cabalExe = LocationUtil.getExeName("cabal");
        if (SystemInfo.isLinux || SystemInfo.isMac) {
            try {
                ProcessLauncher getCabalDir = new ProcessLauncher(true, null, "which", "cabal");
                File cabal = new File(getCabalDir.getStdOut(), cabalExe);
                if (cabal.exists())
                    return cabal.getPath();
            } catch (Exception e) {
                LOG.error(e.getMessage());
            }
        } else if (SystemInfo.isWindows) {
            String separator = System.getProperty("file.separator");
            String libPath = suggestLibPath(sdk);
            if (libPath == null)
                return null;
            File cabalDir = new File(libPath + separator + "extralibs" + separator + "bin");
            File cabal = new File(cabalDir, cabalExe);
            if (cabalDir.isDirectory() && cabal.exists())
                return cabal.getPath();
        }
        return null;
    }

    private static boolean compileHs(@NotNull final Module module, final File pluginPath, VirtualFile ghcHome, File exe) throws IOException, InterruptedException {
        Project project = module.getProject();
        Task haddockBackgroundTask = new Task.Backgroundable(project, "Installing Haddock 2.9.2 if missing", true) {

            public void run(ProgressIndicator indicator) {
                indicator.setText("Checking Haddock installation...");
                indicator.setFraction(0.0);
                HaddockLocation.get(module, indicator);
                indicator.setFraction(1.0);
            }
        }.setCancelText("Stop Haddock installation");
        haddockBackgroundTask.queue();
        exe.delete();
        String ghcExe = LocationUtil.getGhcCommandPath(ghcHome);
        if (ghcExe == null)
            return false;
        StatusBar.Info.set("Compiling " + MAIN_FILE + "...", project);
        try {
            LocationUtil.listHaskellSources(new LocationUtil.HsCallback() {
                public void run(ZipInputStream zis, ZipEntry entry) throws IOException {
                    File outFile = new File(pluginPath, entry.getName());
                    OutputStream os = new FileOutputStream(outFile);
                    try {
                        StreamUtil.copyStreamContent(zis, os);
                    } finally {
                        os.close();
                    }
                }
            });
            String mainHs = MAIN_FILE + ".hs";
            ProcessLauncher launcher = new ProcessLauncher(
                true, null, ghcExe,
                "--make", "-cpp", "-O", "-package", "ghc",
                "-i" + pluginPath.getAbsolutePath(),
                new File(pluginPath, mainHs).getAbsolutePath()
            );
            for (int i = 0; i < 3; i++) {
                if (exe.exists())
                    return true;
                Thread.sleep(100);
            }
            String stdErr = launcher.getStdErr();
            LOG.error("Compiling " + mainHs + ":\n" + stdErr);
            return false;
        } finally {
            StatusBar.Info.set("Done compiling " + MAIN_FILE, project);
        }
    }
}
