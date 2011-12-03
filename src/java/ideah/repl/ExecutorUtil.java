package ideah.repl;

import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;

import java.util.concurrent.*;

final class ExecutorUtil {

    private static ExecutorService ourThreadExecutorsService = null;

    static Future<?> executeOnPooledThread(Runnable task) {
        Application application = ApplicationManager.getApplication();
        if (application != null) {
            return application.executeOnPooledThread(task);
        } else {
            if (ourThreadExecutorsService == null) {
                ourThreadExecutorsService = new ThreadPoolExecutor(
                    10,
                    Integer.MAX_VALUE,
                    60L,
                    TimeUnit.SECONDS,
                    new SynchronousQueue<Runnable>()
                );
            }
            return ourThreadExecutorsService.submit(task);
        }
    }
}
