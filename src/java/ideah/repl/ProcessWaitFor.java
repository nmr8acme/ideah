package ideah.repl;

import com.intellij.util.concurrency.Semaphore;

import java.util.concurrent.Future;

final class ProcessWaitFor {

    private final Semaphore semaphore = new Semaphore();

    private final Future<?> waitForThreadFuture;
    private int exitCode;

    ProcessWaitFor(final Process process) {
        semaphore.down();
        Runnable action = new Runnable() {
            public void run() {
                try {
                    exitCode = process.waitFor();
                } catch (InterruptedException ex) {
                    // Do nothing, by design
                }
                semaphore.up();
            }
        };

        waitForThreadFuture = ExecutorUtil.executeOnPooledThread(action);
    }

    void detach() {
        waitForThreadFuture.cancel(true);
        semaphore.up();
    }

    int waitFor() {
        semaphore.waitFor();
        return exitCode;
    }
}
