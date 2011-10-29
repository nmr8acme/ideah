package ideah.tree;

import java.util.Collections;

public final class WhereBinds extends Located {

    public WhereBinds(IRange location) {
        super(location);
    }

    @Override
    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
