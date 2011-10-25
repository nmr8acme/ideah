package ideah.tree.pat;

import ideah.util.LineColRange;

public final class LazyPat extends Pat {

    public final Pat pattern;

    public LazyPat(LineColRange location, Pat pattern) {
        super(location);
        this.pattern = pattern;
    }
}
