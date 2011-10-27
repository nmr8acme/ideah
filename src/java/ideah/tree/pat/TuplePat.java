package ideah.tree.pat;

import ideah.util.IRange;

import java.util.List;

public final class TuplePat extends Pat {

    public final List<Pat> patterns;

    public TuplePat(IRange location, List<Pat> patterns) {
        super(location);
        this.patterns = patterns;
    }

    protected Iterable<Pat> getChildren() {
        return patterns;
    }
}
