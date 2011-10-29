package ideah.tree.pat;

import ideah.tree.IRange;

import java.util.List;

public final class ListPat extends Pat {

    public final List<Pat> patterns;

    public ListPat(IRange location, List<Pat> patterns) {
        super(location);
        this.patterns = patterns;
    }

    protected Iterable<Pat> getChildren() {
        return patterns;
    }
}
