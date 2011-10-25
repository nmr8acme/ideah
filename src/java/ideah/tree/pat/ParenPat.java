package ideah.tree.pat;

import ideah.util.LineColRange;

public final class ParenPat extends Pat {

    public final Pat pattern;

    public ParenPat(LineColRange location, Pat pattern) {
        super(location);
        this.pattern = pattern;
    }
}
