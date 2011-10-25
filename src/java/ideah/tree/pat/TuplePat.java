package ideah.tree.pat;

import ideah.util.LineColRange;

import java.util.List;

public final class TuplePat extends Pat {

    public final List<Pat> patterns;

    public TuplePat(LineColRange location, List<Pat> patterns) {
        super(location);
        this.patterns = patterns;
    }
}
