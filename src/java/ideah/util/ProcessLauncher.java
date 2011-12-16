package ideah.util;

import com.intellij.openapi.util.io.StreamUtil;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

public final class ProcessLauncher {

    public static final String NEW_MSG_INDICATOR = "\f";

    private final String stdOut;
    private final String stdErr;

    public ProcessLauncher(boolean waitFor, InputStream stdin, List<String> args) throws InterruptedException, IOException {
        this(waitFor, stdin, args.toArray(new String[args.size()]));
    }

    public ProcessLauncher(boolean waitFor, InputStream stdin, String... args) throws InterruptedException, IOException {
        if (args.length > 0) {
            Process process = Runtime.getRuntime().exec(args);
            StreamReader outReader = new StreamReader(process.getInputStream());
            StreamReader errReader = new StreamReader(process.getErrorStream());
            outReader.start();
            errReader.start();
            if (stdin != null) {
                OutputStream os = process.getOutputStream();
                StreamUtil.copyStreamContent(stdin, os);
                os.close();
            }
            errReader.join();
            outReader.join();
            this.stdOut = outReader.getOutput();
            this.stdErr = errReader.getOutput();
            if (waitFor) {
                process.waitFor();
            }
        } else {
            stdOut = null;
            stdErr = null;
        }
    }

    public String getStdOut() {
        return stdOut;
    }

    public String getStdErr() {
        return stdErr;
    }
}
