package ideah.sdk;

import ideah.util.GHCUtil;
import ideah.util.GHCVersion;

final class GHCDir {

    final String name;
    final GHCVersion version;

    GHCDir(String name) {
        this.name = name;
        this.version = GHCUtil.getVersion(name);
    }
}
