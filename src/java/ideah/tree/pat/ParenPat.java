package ideah.tree.pat;

import ideah.util.IRange;

import java.util.Arrays;

public final class ParenPat extends Pat {

    public final Pat pattern;

    public ParenPat(IRange location, Pat pattern) {
        super(location);
        this.pattern = pattern;
    }

    protected Iterable<Pat> getChildren() {
        return Arrays.asList(pattern);
    }
}
