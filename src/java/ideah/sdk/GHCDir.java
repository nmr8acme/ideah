package ideah.sdk;

import ideah.util.LocationUtil;

import java.util.ArrayList;

final class GHCDir {

    final String name;
    final Integer[] version;

    GHCDir(String name) {
        this.name = name;
        ArrayList<Integer> versionList = LocationUtil.getVersion(name);
        version = versionList.toArray(new Integer[versionList.size()]);
    }
}
