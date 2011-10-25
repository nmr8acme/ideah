package ideah.tree.expr;

import ideah.tree.Located;
import ideah.util.LineColRange;

public abstract class Expression extends Located {

    protected Expression(LineColRange location) {
        super(location);
    }
}
