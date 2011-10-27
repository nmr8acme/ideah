package ideah.util;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiFile;

public interface IRange {

    TextRange getRange(PsiFile file);

    ILocation getStart();

    ILocation getEnd();
}
