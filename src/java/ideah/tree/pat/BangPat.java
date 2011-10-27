package ideah.tree.pat;

import ideah.util.LineColRange;

import java.util.Arrays;

public final class BangPat extends Pat {

    public final Pat pattern;

    public BangPat(LineColRange location, Pat pattern) {
        super(location);
        this.pattern = pattern;
    }

    protected Iterable<Pat> getChildren() {
        return Arrays.asList(pattern);
    }
}
