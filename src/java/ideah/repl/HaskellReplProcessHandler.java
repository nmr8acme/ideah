package ideah.repl;

import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessOutputTypes;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.util.Alarm;
import ideah.HaskellFileType;

import java.io.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

final class HaskellReplProcessHandler extends ProcessHandler {

    private static final Logger LOG = Logger.getInstance(HaskellReplProcessHandler.class.getName());

    private final Process process;
    private final ProcessWaitFor waitFor;

    HaskellReplProcessHandler(String path, Module module) throws IOException {
        process = Runtime.getRuntime().exec(""); // todo: run ghci
        waitFor = new ProcessWaitFor(process);
    }

    private Reader createProcessOutReader() {
        return new BufferedReader(new InputStreamReader(process.getInputStream(), HaskellFileType.HASKELL_CHARSET));
    }

    private Reader createProcessErrReader() {
        return new BufferedReader(new InputStreamReader(process.getErrorStream(), HaskellFileType.HASKELL_CHARSET));
    }

    public void startNotify() {
        final ReadProcessThread stdoutThread = new ReadProcessThread(createProcessOutReader()) {
            protected void textAvailable(String s) {
                notifyTextAvailable(s, ProcessOutputTypes.STDOUT);
            }
        };

        final ReadProcessThread stderrThread = new ReadProcessThread(createProcessErrReader()) {
            protected void textAvailable(String s) {
                notifyTextAvailable(s, ProcessOutputTypes.STDERR);
            }
        };

        //notifyTextAvailable(myCommandLine + '\n', ProcessOutputTypes.SYSTEM);

        addProcessListener(new ProcessAdapter() {
            public void startNotified(ProcessEvent event) {
                try {
                    final Future<?> stdOutReadingFuture = ExecutorUtil.executeOnPooledThread(stdoutThread);
                    final Future<?> stdErrReadingFuture = ExecutorUtil.executeOnPooledThread(stderrThread);

                    Runnable action = new Runnable() {
                        public void run() {
                            int exitCode = 0;

                            try {
                                exitCode = waitFor.waitFor();

                                // tell threads that no more attempts to read process' output should be made
                                stderrThread.setProcessTerminated(true);
                                stdoutThread.setProcessTerminated(true);

                                stdErrReadingFuture.get();
                                stdOutReadingFuture.get();
                            } catch (InterruptedException ex) {
                                // Do nothing
                            } catch (ExecutionException ex) {
                                // Do nothing
                            }

                            notifyProcessTerminated(exitCode);
                        }
                    };

                    ExecutorUtil.executeOnPooledThread(action);
                } finally {
                    removeProcessListener(this);
                }
            }
        });

        super.startNotify();
    }

    protected void destroyProcessImpl() {
        try {
            closeStreams();
        } finally {
            process.destroy();
        }
    }

    private void closeStreams() {
        try {
            process.getOutputStream().close();
        } catch (IOException ex) {
            LOG.error(ex);
        }
    }

    protected void detachProcessImpl() {
        Runnable runnable = new Runnable() {
            public void run() {
                closeStreams();

                waitFor.detach();
                notifyProcessDetached();
            }
        };

        ExecutorUtil.executeOnPooledThread(runnable);
    }

    public boolean detachIsDefault() {
        return false;
    }

    public OutputStream getProcessInput() {
        return process.getOutputStream();
    }

    private abstract static class ReadProcessThread implements Runnable {

        private static final int NOTIFY_TEXT_DELAY = 300;

        private final Reader reader;

        private final StringBuilder buffer = new StringBuilder();
        private final Alarm alarm;

        private boolean isClosed = false;
        private boolean isProcessTerminated = false;

        protected ReadProcessThread(Reader reader) {
            this.reader = reader;
            alarm = new Alarm(Alarm.ThreadToUse.SHARED_THREAD);
        }

        synchronized boolean isProcessTerminated() {
            return isProcessTerminated;
        }

        synchronized void setProcessTerminated(boolean isProcessTerminated) {
            this.isProcessTerminated = isProcessTerminated;
        }

        public void run() {
            alarm.addRequest(new Runnable() {
                public void run() {
                    if (!isClosed()) {
                        alarm.addRequest(this, NOTIFY_TEXT_DELAY);
                        checkTextAvailable();
                    }
                }
            }, NOTIFY_TEXT_DELAY);

            try {
                while (!isClosed()) {
                    int c = readNextByte();
                    if (c < 0)
                        break;
                    synchronized (buffer) {
                        buffer.append((char) c);
                    }
                    if (c == '\n') { // not by '\r' because of possible '\n'
                        checkTextAvailable();
                    }
                }
            } catch (Exception ex) {
                LOG.error(ex);
            }

            close();
        }

        private int readNextByte() {
            try {
                while (!reader.ready()) {
                    if (isProcessTerminated())
                        return -1;
                    try {
                        Thread.sleep(1L);
                    } catch (InterruptedException ignore) {
                        // ignore
                    }
                }
                return reader.read();
            } catch (IOException ex) {
                return -1; // When process terminated Process.getInputStream()'s underlaying stream becomes closed on Linux.
            }
        }

        private void checkTextAvailable() {
            synchronized (buffer) {
                if (buffer.length() <= 0)
                    return;
                // warning! Since myBuffer is reused, do not use myBuffer.toString() to fetch the string
                // because the created string will get StringBuffer's internal char array as a buffer which is possibly too large.
                String s = buffer.substring(0, buffer.length());
                buffer.setLength(0);
                textAvailable(s);
            }
        }

        private void close() {
            synchronized (this) {
                if (isClosed())
                    return;
                isClosed = true;
            }
            try {
                reader.close();
            } catch (IOException ex) {
                // ignore
            }
            checkTextAvailable();
        }

        protected abstract void textAvailable(String s);

        private synchronized boolean isClosed() {
            return isClosed;
        }
    }
}
