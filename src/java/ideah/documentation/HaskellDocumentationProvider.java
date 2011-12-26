package ideah.documentation;

import com.intellij.lang.documentation.DocumentationProvider;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import ideah.psi.api.HPIdent;
import ideah.util.*;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.List;

public final class HaskellDocumentationProvider implements DocumentationProvider {

    private static final Logger LOG = Logger.getInstance("ideah.documentation.HaskellDocumentationProvider");

    public String getQuickNavigateInfo(PsiElement element, PsiElement originalElement) {
        return null;
    }

    public List<String> getUrlFor(PsiElement element, PsiElement originalElement) {
        return null; // todo: find external docs
    }

    public String generateDoc(PsiElement element, PsiElement originalElement) {
        if (!(element instanceof HPIdent))
            return null;
        HPIdent ident = (HPIdent) element;
        TextRange range = ident.getTextRange();
        PsiFile psiFile = element.getContainingFile();
        VirtualFile file = psiFile.getVirtualFile();
        if (file == null)
            return null;
        // todo: run in event-dispatch thread
        //fdm.saveAllDocuments();
        // try this: PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
        int offset = range.getStartOffset();
        LineCol coord = LineCol.fromOffset(psiFile, offset);
        Module module = DeclarationPosition.getDeclModule(psiFile);
        if (module == null)
            return null;
        CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null) {
            return null;
        }
        try {
            String sourcePath = GHCUtil.rootsAsString(module, false);
            List<String> args = compiler.getCompileOptionsList(
                "-m", "GetIdType",
                "-s", sourcePath,
                "--line-number", String.valueOf(coord.line), "--column-number", String.valueOf(coord.column),
                file.getPath()
            );
            ProcessLauncher idLauncher = new ProcessLauncher(false, null, args);
            String stdOut = idLauncher.getStdOut();
            if (stdOut.trim().isEmpty())
                return null;
            String newMsgIndicator = ProcessLauncher.NEW_MSG_INDICATOR;
            int p = stdOut.indexOf(newMsgIndicator);
            String modName;
            String type;
            if (p >= 0) {
                modName = stdOut.substring(0, p).trim();
                type = stdOut.substring(p + 1).trim();
            } else {
                modName = "?";
                type = "?";
            }

            StringBuilder documentation = new StringBuilder(
                (modName.length() == 0
                    ? ""
                    : "Module: <code>" + modName + "</code><br>")
                    + "Type: <code>" + type + "</code><br>");
            DeclarationPosition declaration = DeclarationPosition.get(psiFile, coord);
            if (declaration != null) {
                ProcessLauncher documentationLauncher = new ProcessLauncher(
                    false, null,
                    HaddockLocation.get(module, null).exe,
                    "-g", compiler.libPath,
                    "-s", sourcePath,
                    "--line-number",
                    String.valueOf(declaration.coord.line),
                    "--column-number", String.valueOf(declaration.coord.column),
                    "--module", declaration.module
                );
                BufferedReader reader = new BufferedReader(new StringReader(documentationLauncher.getStdOut()));
                while (true) {
                    String l = reader.readLine();
                    if (l == null || l.startsWith(newMsgIndicator))
                        break;
                }
                String newLineHtml = "<br>";
                while (true) {
                    String l = reader.readLine();
                    if (l != null) {
                        documentation.append(l).append(newLineHtml);
                    } else break;
                }
                documentation.substring(0, documentation.lastIndexOf(newLineHtml));
            }
            return documentation.toString();
        } catch (Exception ex) {
            LOG.error(ex);
            return null;
        }
    }

    public PsiElement getDocumentationElementForLookupItem(PsiManager psiManager, Object object, PsiElement element) {
        return null;
    }

    public PsiElement getDocumentationElementForLink(PsiManager psiManager, String link, PsiElement context) {
        return null;
    }
}
