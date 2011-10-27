package ideah.tree.expr;

import com.google.common.collect.Iterables;
import ideah.tree.LocalBinds;
import ideah.tree.Located;
import ideah.util.LineColRange;

import java.util.Arrays;

public final class LetExpr extends Expression {

    public final LocalBinds localBinds;
    public final Expression expression;

    public LetExpr(LineColRange location, LocalBinds localBinds, Expression expression) {
        super(location);
        this.localBinds = localBinds;
        this.expression = expression;
    }

    protected Iterable<Located> getChildren() {
        return Iterables.concat(localBinds.getChildren(), Arrays.asList(expression));
    }
}
