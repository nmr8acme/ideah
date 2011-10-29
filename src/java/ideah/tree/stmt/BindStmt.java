package ideah.tree.stmt;

import ideah.tree.IRange;
import ideah.tree.Located;
import ideah.tree.expr.Expression;
import ideah.tree.pat.Pat;

import java.util.Arrays;

public final class BindStmt extends Statement {

    public final Pat pattern;
    public final Expression expression;

    public BindStmt(IRange location, Pat pattern, Expression expression) {
        super(location);
        this.pattern = pattern;
        this.expression = expression;
    }

    protected Iterable<Located> getChildren() {
        return Arrays.asList(pattern, expression);
    }
}
