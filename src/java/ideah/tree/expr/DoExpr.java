package ideah.tree.expr;

import com.google.common.collect.Iterables;
import ideah.tree.Located;
import ideah.tree.stmt.Statement;
import ideah.util.LineColRange;

import java.util.Arrays;
import java.util.List;

public final class DoExpr extends Expression {

    public final List<Statement> statements;
    public final Expression expression;

    public DoExpr(LineColRange location, List<Statement> statements, Expression expression) {
        super(location);
        this.statements = statements;
        this.expression = expression;
    }

    protected Iterable<Located> getChildren() {
        return Iterables.concat(statements, Arrays.asList(expression));
    }
}
