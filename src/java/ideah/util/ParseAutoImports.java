package ideah.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

public final class ParseAutoImports {

    public static Map<String, SortedSet<String>> parseAutoImports(String stdOut) throws IOException {
        BufferedReader rdr = new BufferedReader(new StringReader(stdOut));
        Map<String, SortedSet<String>> autoImports = new HashMap<String, SortedSet<String>>();
        while (true) {
            String line = rdr.readLine();
            if (line == null)
                break;
            line = line.trim();
            int p = line.lastIndexOf('.');
            if (p < 0)
                continue;
            String symbol = line.substring(p + 1);
            String module = line.substring(0, p);
            SortedSet<String> modules = autoImports.get(symbol);
            if (modules == null) {
                modules = new TreeSet<String>();
                autoImports.put(symbol, modules);
            }
            modules.add(module);
        }
        return autoImports;
    }
}
