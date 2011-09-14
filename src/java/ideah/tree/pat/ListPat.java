package ideah.tree.pat;

import java.util.List;

public final class ListPat extends Pat {

    public final List<Pat> patterns;

    public ListPat(List<Pat> patterns) {
        this.patterns = patterns;
    }
}
