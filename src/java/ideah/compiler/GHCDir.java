package ideah.compiler;

public final class GHCDir {

    public final String name;
    Integer[] version;

    public Integer[] getVersion() {
        return version;
    }

    public GHCDir(String name) {
        this.name = name;
        String[] versionStr = name.split("[^0-9]");
        version = new Integer[versionStr.length];
        for (int i = 0; i < versionStr.length; i++) {
            try {
                version[i] = Integer.parseInt(versionStr[i]);
            } catch (NumberFormatException e) {
                Integer[] temp = new Integer[i];
                System.arraycopy(version, 0, temp, 0, i);
                version = temp;
                return;
            }
        }
    }
}
