package ideah.tree;

import java.util.Collections;

public final class Ident extends Located {

    public Ident(IRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
