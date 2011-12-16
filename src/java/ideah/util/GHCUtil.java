package ideah.util;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public final class GHCUtil {

    public static String getExeName(String file) {
        return SystemInfo.isWindows
            ? file + ".exe"
            : file;
    }

    public static String getGhcCommandPath(VirtualFile ghcHome) {
        if (ghcHome == null)
            return null;
        VirtualFile virBin = ghcHome.findChild("bin");
        if (virBin == null)
            return null;
        return new File(virBin.getPath(), "ghc").getAbsolutePath();
    }

    public static String rootsAsString(@NotNull Module module, boolean tests) {
        VirtualFile[] sourceRoots = ModuleRootManager.getInstance(module).getSourceRoots(tests);
        StringBuilder buf = new StringBuilder();
        for (VirtualFile root : sourceRoots) {
            buf.append(':').append(root.getPath());
        }
        return buf.substring(1);
    }

    public static List<Integer> getVersion(@NotNull String name) {
        String[] versionStr = name.split("[^0-9]");
        List<Integer> parts = new ArrayList<Integer>();
        for (String part : versionStr) {
            if (part.isEmpty())
                continue;
            try {
                parts.add(new Integer(part));
            } catch (NumberFormatException nfex) {
                // ignore
            }
        }
        return parts;
    }
}
