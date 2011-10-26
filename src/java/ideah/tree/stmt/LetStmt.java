package ideah.tree.stmt;

import ideah.tree.LocalBinds;
import ideah.util.LineColRange;

public final class LetStmt extends Statement {

    public final LocalBinds binds;

    public LetStmt(LineColRange location, LocalBinds binds) {
        super(location);
        this.binds = binds;
    }
}
