package ideah.tree.stmt;

import ideah.tree.LocalBinds;
import ideah.tree.Located;
import ideah.util.IRange;

public final class LetStmt extends Statement {

    public final LocalBinds binds;

    public LetStmt(IRange location, LocalBinds binds) {
        super(location);
        this.binds = binds;
    }

    protected Iterable<? extends Located> getChildren() {
        return binds.getChildren();
    }
}
