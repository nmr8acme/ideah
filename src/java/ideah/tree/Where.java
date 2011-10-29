package ideah.tree;

import com.intellij.formatting.Indent;

import java.util.Collections;

public final class Where extends Located {

    public Where(IRange location) {
        super(location);
    }

    @Override
    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }

    @Override
    protected void format() {
        indent = Indent.getNormalIndent();
    }
}
