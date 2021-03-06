package ideah.tree.pat;

import ideah.tree.IRange;
import ideah.tree.Located;

import java.util.Collections;

public final class WildPat extends Pat {

    public WildPat(IRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
