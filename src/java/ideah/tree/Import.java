package ideah.tree;

import java.util.Collections;

public final class Import extends Located {

    // todo

    public Import(IRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList(); // todo
    }
}
