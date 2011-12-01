package ideah.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiReference;
import com.intellij.util.IncorrectOperationException;
import ideah.psi.api.HPIdent;
import ideah.util.DeclarationPosition;
import ideah.util.LineCol;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public final class HPIdentImpl extends HaskellBaseElementImpl implements HPIdent, PsiReference {

    private static final Logger LOG = Logger.getInstance("ideah.psi.impl.HPIdentImpl");

    public HPIdentImpl(@NotNull ASTNode node) {
        super(node);
    }

    @Override
    public PsiReference getReference() {
        return this;
    }

    public PsiElement setName(@NotNull @NonNls String name) throws IncorrectOperationException {
        return null; // todo
    }

    public PsiElement getElement() {
        return this;
    }

    public TextRange getRangeInElement() {
        return new TextRange(0, getTextLength());
    }

    public PsiElement resolve() {
        PsiFile psiFile = getContainingFile();
        try {
            return getElementAt(getProject(), DeclarationPosition.get(psiFile, LineCol.fromOffset(psiFile, getTextOffset())));
        } catch (Exception e) {
            LOG.error(e.getMessage());
            return null;
        }
    }

    public static PsiElement getElementAt(Project project, DeclarationPosition declaration) {
        if (declaration == null)
            return null;
        VirtualFile baseDir = project.getBaseDir();
        if (baseDir == null)
            return null;
        VirtualFile declarationModuleVirtualFile = baseDir.getFileSystem().findFileByPath(declaration.module);
        if (declarationModuleVirtualFile == null)
            return null;
        PsiFile declarationModulePsiFile = PsiManager.getInstance(project).findFile(declarationModuleVirtualFile);
        if (declarationModulePsiFile == null)
            return null;
        LineCol coord = declaration.coord;
        if (coord == null)
            return null;
        try {
            PsiElement elementAt = declarationModulePsiFile.getViewProvider().findElementAt(coord.getOffset(declarationModulePsiFile));
            if (elementAt == null)
                return null;
            return new HPIdentImpl(elementAt.getNode());
        } catch (ProcessCanceledException ex) {
            return null;
        } catch (Exception e) {
            LOG.error(e);
            return null;
        }
    }

    @NotNull
    public String getCanonicalText() {
        return getText(); // todo: resolve full module name?
    }

    public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
        return null; // todo
    }

    public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException {
        return null; // todo
    }

    public boolean isReferenceTo(PsiElement element) {
        return false; // todo
    }

    @NotNull
    public Object[] getVariants() {
        return new Object[0]; // todo
    }

    public boolean isSoft() {
        return false;
    }
}
