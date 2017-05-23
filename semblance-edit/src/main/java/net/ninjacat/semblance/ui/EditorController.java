package net.ninjacat.semblance.ui;

import com.google.common.base.Charsets;
import com.google.common.io.Files;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
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
    @FXML
    private TextArea consoleOut;
    @FXML
    private TextField consoleIn;
    @FXML
    private Label position;
    @FXML
    private Label messageBox;
    @FXML
    private AnchorPane editorStatus;

    private Interpreter interpreter = null;

    @Override
    public void initialize(final URL location, final ResourceBundle resources) {
        webView.getEngine().loadContent(loadEditorTemplate());

        webView.autosize();
        consoleIn.autosize();
        consoleOut.autosize();
        editorStatus.autosize();

        runButton.setOnAction(event -> runScript());
    }

    public void clearErrorLine() {
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

    public LispValue runScript() {
        clearErrorLine();
        messageBox.setVisible(false);
        editorStatus.autosize();
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
        if (!sourceInfo.equals(SourceInfo.UNKNOWN)) {
            setErrorLine(sourceInfo.getLine());
            setCursor(sourceInfo.getLine(), sourceInfo.getPosition());

            webView.requestFocus();
        }
        if (message != null && !message.trim().isEmpty()) {
            messageBox.setText(message);
            messageBox.setVisible(true);
        } else {
            messageBox.setVisible(false);
        }
        editorStatus.autosize();
    }
}
