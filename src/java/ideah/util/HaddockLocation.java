package ideah.util;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.progress.ProgressIndicator;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.*;

public final class HaddockLocation {

    private static final Logger LOG = Logger.getInstance("ideah.util.HaddockLocation");
    private static final String MAIN_FILE = "ask_haddock";

    public final String exe;

    private HaddockLocation(String exe) {
        this.exe = exe;
    }

    private static List<String> getMissingPackages(@NotNull String cabalPath, String... packages) throws IOException, InterruptedException {
        List<String> args = new ArrayList<String>();
        args.addAll(Arrays.asList(cabalPath, "list", "--installed", "-v0", "--simple-output"));
        TreeMap<String, String> argsPackages = new TreeMap<String, String>();
        for (String pkg : packages) {
            argsPackages.put(pkg, getPackageBaseName(pkg));
        }
        args.addAll(argsPackages.values()); // For "haddock list --installed <package name>", the package name should not include the version
        List<String> packageList = new ArrayList<String>(Arrays.asList(packages));
        Collections.sort(packageList);
        List<String> missingPackages = new ArrayList<String>();
        Iterator<String> iterator = packageList.iterator();
        ProcessLauncher getMissingPackagesLauncher = new ProcessLauncher(true, null, args);
        BufferedReader reader = new BufferedReader(new StringReader(getMissingPackagesLauncher.getStdOut()));
        String line = reader.readLine();
        while (line != null && iterator.hasNext()) {
            String next = iterator.next();
            if (line.startsWith(argsPackages.get(next)) && GHCUtil.getVersion(next).equals(GHCUtil.getVersion(line))) {
                line = reader.readLine();
            } else {
                missingPackages.add(next);
            }
        }
        while (iterator.hasNext()) {
            missingPackages.add(iterator.next());
        }
        return missingPackages;
    }

    private static String getPackageBaseName(String pkg) {
        return pkg.substring(0, pkg.lastIndexOf('-'));
    }

    private static void runCabal(@NotNull String cabalPath, List<String> cabalArgsList) throws IOException, InterruptedException {
        List<String> args = new ArrayList<String>();
        args.add(cabalPath);
        args.addAll(cabalArgsList);
        new ProcessLauncher(
            true, null, args
        );
    }

    private static void cabalInstall(@NotNull String cabalPath, @Nullable ProgressIndicator indicator, double maxIndicatorFraction, @NotNull List<String> packages) throws IOException, InterruptedException {
        if (packages.isEmpty())
            return;
        GHCUtil.updateIndicatorText(indicator, "Updating Cabal...");
        runCabal(cabalPath, Arrays.asList("update"));
        double fractionRange = getFractionRange(indicator, maxIndicatorFraction);
        int size = packages.size();
        GHCUtil.increaseIndicatorFraction(indicator, fractionRange / (size + 1));
        if (indicator == null) {
            List<String> cabalArgsList = new ArrayList<String>();
            cabalArgsList.add("install");
            cabalArgsList.addAll(packages);
            runCabal(cabalPath, cabalArgsList);
        } else {
            double step = getFractionRange(indicator, maxIndicatorFraction) / size;
            for (String pkg : packages) {
                GHCUtil.updateIndicatorText(indicator, "Installing package " + getPackageBaseName(pkg) + "...");
                runCabal(cabalPath, Arrays.asList("install", pkg));
                GHCUtil.increaseIndicatorFraction(indicator, step);
            }
        }
    }

    // todo: HTTP proxy settings
    private static void cabalCheckAndInstall(@NotNull String cabalPath, @Nullable ProgressIndicator indicator, double maxIndicatorFraction, String... packages) {
        if (packages.length > 0) {
            try {
                GHCUtil.updateIndicatorText(indicator, "Checking installed Cabal packages...");
                List<String> missingPackages = getMissingPackages(cabalPath, packages);
                double fractionRange = getFractionRange(indicator, maxIndicatorFraction);
                GHCUtil.increaseIndicatorFraction(indicator, fractionRange / (packages.length + 1));
                cabalInstall(cabalPath, indicator, maxIndicatorFraction, missingPackages);
            } catch (Exception e) {
                LOG.error(e);
            }
        }
    }

    private static double getFractionRange(ProgressIndicator indicator, double maxIndicatorFraction) {
        return indicator == null ? 0 : maxIndicatorFraction - indicator.getFraction();
    }

    public static synchronized HaddockLocation get(@Nullable Module module, @Nullable ProgressIndicator indicator) {
        AskUtil ask = AskUtil.get(module, MAIN_FILE);
        if (ask == null)
            return null;
        String cabalPath = ask.getCabalPath();
        if (cabalPath == null)
            return null;
        try {
            if (ask.needRecompile()) {
                cabalCheckAndInstall(cabalPath, indicator, 0.7, "haddock-2.9.2");
                if (!ask.compileHs(indicator, 1.0))
                    return null;
            }
            File exe = ask.getExe();
            if (exe != null) {
                return new HaddockLocation(exe.getAbsolutePath());
            } else {
                return null;
            }
        } catch (Exception ex) {
            LOG.error(ex);
            return null;
        }
    }
}
