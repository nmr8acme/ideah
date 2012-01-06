package ideah.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class LocationUtil {

    private final String exe;
    private final String libPath;
    private final String ghcOptions;

    LocationUtil(String exe, String libPath, String ghcOptions) {
        this.exe = exe;
        this.libPath = libPath;
        this.ghcOptions = ghcOptions;
    }

    private static void append(StringBuilder buf, String str) {
        if (str != null && str.length() > 0) {
            if (buf.length() > 0) {
                buf.append(' ');
            }
            buf.append(str);
        }
    }

    public List<String> getCompileOptionsList(String... additionalArgs) {
        List<String> args = new ArrayList<String>();
        args.add(exe);
        args.addAll(Arrays.asList("-g", libPath));
        StringBuilder buf = new StringBuilder();
        append(buf, ghcOptions);
        if (buf.length() > 0) {
            args.addAll(Arrays.asList("-c", buf.toString()));
        }
        args.addAll(Arrays.asList(additionalArgs));
        return args;
    }
}
