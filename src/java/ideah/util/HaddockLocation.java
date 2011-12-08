package ideah.util;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
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

public final class HaddockLocation {

    private static final Logger LOG = Logger.getInstance("ideah.util.HaddockLocation");
    private static final String MAIN_FILE = "ask_haddock";

    public final String exe;

    private HaddockLocation(String exe) {
        this.exe = exe;
    }

    public static synchronized HaddockLocation get(@Nullable Module module, @Nullable ProgressIndicator indicator) {
        LocationUtil.cabalCheckAndInstall(indicator, "haddock-2.9.2");
        if (module == null)
            return null;
        Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
        if (sdk == null)
            return null;
        VirtualFile ghcHome = sdk.getHomeDirectory();
        if (ghcHome == null)
            return null;
        try {
            // todo: cache result somewhere (or better store in SDK settings)
            String ghcCommandPath = LocationUtil.getGhcCommandPath(ghcHome);
            if (ghcCommandPath == null)
                return null;
        } catch (Exception e) {
            LOG.error(e);
        }
        try {
            File pluginPath = new File(new File(System.getProperty("user.home"), ".ideah"), sdk.getVersionString());
            pluginPath.mkdirs();
            File haddockExe = new File(pluginPath, LocationUtil.getExeName(MAIN_FILE));
            if (LocationUtil.needRecompile(haddockExe)) {
                if (!compileHs(module.getProject(), pluginPath, ghcHome, haddockExe))
                    return null;
            }
            if (haddockExe.exists())
                return new HaddockLocation(haddockExe.getAbsolutePath());
            else
                return null;
        } catch (Exception ex) {
            LOG.error(ex);
            return null;
        }
    }

    private static boolean compileHs(@Nullable Project project, @NotNull final File pluginPath, @Nullable VirtualFile ghcHome, File exe) throws IOException, InterruptedException { // todo: not sure about @NotNull
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
