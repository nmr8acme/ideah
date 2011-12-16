package ideah.util;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;
import java.util.concurrent.atomic.AtomicBoolean;

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

    public static synchronized CompilerLocation get(@Nullable final Module module) {
        if (module == null)
            return null;
        final AskUtil ask = AskUtil.get(module, MAIN_FILE);
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
                FutureTask<Boolean> task = new FutureTask<Boolean>(new Callable<Boolean>() {
                    public Boolean call() throws Exception {
                        // call() is invoked in Event Dispatch Thread
                        final AtomicBoolean exeExists = new AtomicBoolean();
                        ProgressManager.getInstance().runProcessWithProgressSynchronously(new Runnable() {
                            public void run() {
                                // run() is invoked in worker thread
                                try {
                                    ProgressIndicator indicator =
                                        ProgressManager.getInstance().getProgressIndicator();
                                    indicator.setText("Preparing " + MAIN_FILE + " compilation...");
                                    indicator.setFraction(0.1);
                                    exeExists.set(ask.compileHs(indicator, 1.0));
                                } catch (Exception e) {
                                    LOG.error(e.getMessage());
                                }
                            }
                        }, "Compiling " + MAIN_FILE, true, module.getProject());
                        return exeExists.get();
                    }
                });
                ApplicationManager.getApplication().invokeLater(task);
                Boolean exeExists = task.get();
                if (!exeExists.booleanValue())
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
        final Task haddockBackgroundTask = new Task.Backgroundable(project, "Installing Haddock if missing", true) {
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
