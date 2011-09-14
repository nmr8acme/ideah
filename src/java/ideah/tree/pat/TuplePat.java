package ideah.tree.pat;

import java.util.List;

public final class TuplePat extends Pat {

    public final List<Pat> patterns;

    public TuplePat(List<Pat> patterns) {
        this.patterns = patterns;
    }
}
