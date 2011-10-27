package ideah.tree.pat;

import ideah.tree.Located;
import ideah.util.LineColRange;

import java.util.Collections;

public final class WildPat extends Pat {

    public WildPat(LineColRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
