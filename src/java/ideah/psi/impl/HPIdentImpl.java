package ideah.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiReference;
import com.intellij.util.IncorrectOperationException;
import ideah.psi.api.HPIdent;
import ideah.util.DeclarationPosition;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

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
        VirtualFile virtualFile = psiFile.getVirtualFile();
        if (virtualFile != null && ModuleRootManager.getInstance(ProjectRootManager.getInstance(psiFile.getProject()).getFileIndex().getModuleForFile(virtualFile)).getSdk() != null) {
            Document thisDoc = FileDocumentManager.getInstance().getCachedDocument(virtualFile);
            if (thisDoc != null) {
                int startOffset = getTextOffset();
                int startLine = thisDoc.getLineNumber(startOffset);
                int startCol = startOffset - thisDoc.getLineStartOffset(startLine);
                try {
                    DeclarationPosition declaration = new DeclarationPosition(startLine, startCol, psiFile);
                    if (declaration.module != null) {
                        Project project = getProject();
                        VirtualFile baseDir = project.getBaseDir();
                        if (baseDir != null) {
                            VirtualFile declarationModuleVirtualFile = baseDir.getFileSystem().findFileByPath(declaration.module);
                            if (declarationModuleVirtualFile != null) {
                                PsiFile declarationModulePsiFile = PsiManager.getInstance(project).findFile(declarationModuleVirtualFile);
                                Document declarationDoc = FileDocumentManager.getInstance().getCachedDocument(declarationModuleVirtualFile);
                                if (declarationDoc != null && declarationModulePsiFile != null) {
                                    int declarationStart = declarationDoc.getLineStartOffset(declaration.startLine - 1) + declaration.startCol - 1;
                                    PsiElement elementAt = declarationModulePsiFile.getViewProvider().findElementAt(declarationStart);
                                    if (elementAt != null)
                                        return new HPIdentImpl(elementAt.getNode());
                                }
                            }
                        }
                    }
                } catch (Exception e) {
                    LOG.error(e);
                }
            }
        }
        return null;
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
