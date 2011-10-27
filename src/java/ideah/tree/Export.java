package ideah.tree;

import ideah.util.IRange;

import java.util.Collections;

public final class Export extends Located {

    // todo

    public Export(IRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList(); // todo
    }
}
