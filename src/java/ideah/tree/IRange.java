package ideah.tree;

import com.intellij.openapi.util.TextRange;

public interface IRange {

    TextRange getRange();

    ILocation getStart();

    ILocation getEnd();
}
