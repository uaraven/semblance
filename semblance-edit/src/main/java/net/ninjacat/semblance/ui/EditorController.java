package net.ninjacat.semblance.ui;

import com.google.common.base.Charsets;
import com.google.common.io.Files;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.web.WebView;
import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;

import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ResourceBundle;
import java.util.stream.Collectors;

public class EditorController implements Initializable {

    @FXML
    private WebView webView;

    @FXML
    private Button runButton;

    private Interpreter interpreter = null;

    public void clearErrorLine(final int lineNumber) {
        webView.getEngine().executeScript("clearErrorHighlighting()");
    }

    public void setErrorLine(final int lineNumber) {
        webView.getEngine().executeScript("highlightError(" + lineNumber + ")");
    }

    public void setCursor(final int line, final int pos) {
        webView.getEngine().executeScript(String.format("editor.setCursor(%s, %s)", line, pos));
    }

    public String getText() {
        return webView.getEngine().executeScript("editor.getValue()").toString();
    }

    @Override
    public void initialize(final URL location, final ResourceBundle resources) {
        webView.autosize();
        webView.getEngine().loadContent(loadEditorTemplate());

        runButton.setOnAction(event -> runScript());
    }

    public LispValue runScript() {
        try {
            final String program = getText();
            return this.interpreter.run(program);
        } catch (final ParsingException e) {
            highlightError(e.getSourceInfo(), e.getMessage());
        } catch (final SemblanceRuntimeException re) {
            highlightError(re.getSourceInfo(), re.getMessage());
        }
        return NilCollection.INSTANCE;
    }

    public void setInterpreter(final Interpreter interpreter) {
        this.interpreter = interpreter;
    }

    /**
     * applies the editing template to the editing code to create the html+javascript source for a code editor.
     */
    private String loadEditorTemplate() {
        try {
            final Path path = Paths.get(getClass().getResource("editor.html").toURI());
            return Files.readLines(path.toFile(), Charsets.UTF_8).stream().collect(Collectors.joining("\n"));
        } catch (final Exception ex) {
            return "";
        }
    }

    private void highlightError(final SourceInfo sourceInfo, final String message) {
        if (sourceInfo.equals(SourceInfo.UNKNOWN)) {

        } else {
            setErrorLine(sourceInfo.getLine());
            setCursor(sourceInfo.getLine(), sourceInfo.getPosition());
        }
    }
}
