package ideah.tree.expr;

import ideah.tree.Located;
import ideah.util.IRange;

public abstract class Expression extends Located {

    protected Expression(IRange location) {
        super(location);
    }
}
