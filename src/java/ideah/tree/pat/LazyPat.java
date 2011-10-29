package ideah.tree.pat;

import ideah.tree.IRange;

import java.util.Arrays;

public final class LazyPat extends Pat {

    public final Pat pattern;

    public LazyPat(IRange location, Pat pattern) {
        super(location);
        this.pattern = pattern;
    }

    protected Iterable<Pat> getChildren() {
        return Arrays.asList(pattern);
    }
}
