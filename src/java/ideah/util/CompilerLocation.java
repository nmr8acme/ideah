package ideah.util;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.Nullable;

import java.io.File;

public final class CompilerLocation {

    private static final Logger LOG = Logger.getInstance("ideah.util.CompilerLocation");
    private static final String MAIN_FILE = "ask_ghc";

    private static boolean askHaddockChecked = false;

    public final String exe;
    public final String libPath;

    private CompilerLocation(String exe, String libPath) {
        this.exe = exe;
        this.libPath = libPath;
    }

    public static synchronized CompilerLocation get(@Nullable Module module) {
        AskUtil ask = AskUtil.get(module, MAIN_FILE);
        if (ask == null)
            return null;
        if (!askHaddockChecked) {
            compileAskHaddock(module);
            askHaddockChecked = true;
        }
        String ghcLib = ask.getLibDir();
        if (ghcLib == null)
            return null;
        try {
            if (ask.needRecompile()) {
                // todo: show foreground dialog
                if (!ask.compileHs())
                    return null;
            }
            File exe = ask.getExe();
            if (exe != null) {
                return new CompilerLocation(exe.getAbsolutePath(), ghcLib);
            } else {
                return null;
            }
        } catch (Exception ex) {
            LOG.error(ex);
            return null;
        }
    }

    private static void compileAskHaddock(final Module module) {
        Project project = module.getProject();
        final Task haddockBackgroundTask = new Task.Backgroundable(project, "Installing Haddock 2.9.2 if missing", true) {
            public void run(ProgressIndicator indicator) {
                indicator.setText("Checking Haddock installation...");
                indicator.setFraction(0.0);
                HaddockLocation.get(module, indicator);
                indicator.setFraction(1.0);
            }
        }.setCancelText("Stop Haddock installation");
        ApplicationManager.getApplication().invokeLater(new Runnable() {
            public void run() {
                haddockBackgroundTask.queue();
            }
        });
    }
}
