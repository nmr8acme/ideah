package ideah.sdk;

import ideah.util.GHCUtil;

import java.util.List;

final class GHCDir {

    final String name;
    final Integer[] version;

    GHCDir(String name) {
        this.name = name;
        List<Integer> versionList = GHCUtil.getVersion(name);
        version = versionList.toArray(new Integer[versionList.size()]);
    }
}
