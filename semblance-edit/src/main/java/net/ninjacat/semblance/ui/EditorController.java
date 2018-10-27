package net.ninjacat.semblance.ui;

import javafx.concurrent.Worker;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextBuilder;
import javafx.scene.text.TextFlow;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.ContextModifier;
import net.ninjacat.semblance.java.Lambdas;
import netscape.javascript.JSObject;

import javax.annotation.Nonnull;
import java.net.URL;
import java.util.ResourceBundle;

import static java.util.stream.Collectors.joining;
import static net.ninjacat.semblance.utils.Values.string;
import static net.ninjacat.semblance.utils.Values.symbol;

public class EditorController implements Initializable {

    @FXML
    private WebView webView;
    @FXML
    private Button runButton;
    @FXML
    private TextFlow consoleOut;
    @FXML
    private TextField consoleIn;
    @FXML
    private Label position;
    @FXML
    private AnchorPane editorStatus;
    @FXML
    private ScrollPane consoleScroll;
    @FXML
    private ListView<Text> messagesList;

    private Interpreter interpreter = null;

    @Override
    public void initialize(final URL location, final ResourceBundle resources) {
        final WebEngine webEngine = this.webView.getEngine();
        loadEditorTemplate();

        this.webView.autosize();
        this.editorStatus.autosize();
        this.consoleScroll.autosize();

        this.consoleScroll.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER);
        this.consoleScroll.setVbarPolicy(ScrollPane.ScrollBarPolicy.AS_NEEDED);

        this.consoleIn.prefWidthProperty().bind(this.consoleScroll.widthProperty().subtract(4));

        this.runButton.setOnAction(event -> runScript());

        webEngine.getLoadWorker().stateProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue == Worker.State.SUCCEEDED) {
                final JSObject window = (JSObject) webEngine.executeScript("window");
                final EditorAccess access = new EditorAccess();
                window.setMember("ui", access);
                final JSObject cursorPos = (JSObject) webEngine.executeScript("editor.getCursor()");
                access.positionChanged((int) cursorPos.getMember("line"), (int) cursorPos.getMember("ch"));
            }
        });

    }

    private void clearErrorLine() {
        this.webView.getEngine().executeScript("clearErrorHighlighting()");
    }

    private void setErrorLine(final int lineNumber) {
        this.webView.getEngine().executeScript("highlightError(" + lineNumber + ")");
    }

    private void setCursor(final int line, final int pos) {
        this.webView.getEngine().executeScript(String.format("editor.setCursor(%s, %s)", line, pos));
    }

    private String getText() {
        return this.webView.getEngine().executeScript("editor.getValue()").toString();
    }

    private LispValue runScript() {
        clearErrorLine();
        this.messagesList.getItems().clear();
        this.editorStatus.autosize();
        try {
            final String program = getText();
            final LispValue result = this.interpreter.run(program);
            final Text resultText = new Text("Result: " + result.repr());
            this.messagesList.getItems().add(resultText);
        } catch (final ParsingException e) {
            highlightError(e.getSourceInfo(), e.getMessage());
        } catch (final SemblanceRuntimeException re) {
            highlightError(re.getSourceInfo(), re.getMessage());
        } catch (final Exception ex) {
            highlightError(SourceInfo.UNKNOWN, ex.getMessage());
        }
        return NilCollection.INSTANCE;
    }

    void setInterpreter(final Interpreter interpreter) {
        this.interpreter = interpreter;
        this.interpreter.injectContextModifier(new ConsoleContextModifier());
    }

    /**
     * applies the editing template to the editing code to create the html+javascript source for a code editor.
     */
    private void loadEditorTemplate() {
        final String url = getClass().getResource("/web/editor.html").toExternalForm();
        this.webView.getEngine().load(url);
    }

    private void highlightError(final SourceInfo sourceInfo, final String message) {
        final String position;
        if (!sourceInfo.equals(SourceInfo.UNKNOWN)) {
            setErrorLine(sourceInfo.getLine());
            setCursor(sourceInfo.getLine(), sourceInfo.getPosition());
            position = String.format("[%d:%d] ", sourceInfo.getLine(), sourceInfo.getPosition());

            this.webView.requestFocus();
        } else {
            position = "";
        }
        if (message != null && !message.trim().isEmpty()) {
            final Text errorText = TextBuilder.create().text(position + message).fill(Color.FIREBRICK).build();
            this.messagesList.getItems().add(errorText);
        } else {
            this.messagesList.getItems().clear();
        }
        this.editorStatus.autosize();
    }

    public class EditorAccess {
        void positionChanged(final int col, final int row) {
            EditorController.this.position.setText(String.format("%d:%d", col + 1, row + 1));
        }
    }

    private class ConsoleContextModifier implements ContextModifier {

        @Override
        public void modify(@Nonnull final Context context) {
            context.bind(symbol("print"), Lambdas.methodAsFunction(this::print));
            context.bind(symbol("println"), Lambdas.methodAsFunction(this::println));
        }

        LispValue print(final Context context, final LispCollection parameters) {
            final String outString = parameters.stream().map(LispValue::printIt).collect(joining(" "));
            return doPrint(outString);
        }

        LispValue println(final Context context, final LispCollection parameters) {
            final String outString = parameters.stream().map(LispValue::printIt).collect(joining(" ")) + "\n";
            return doPrint(outString);
        }

        private LispValue doPrint(final String value) {
            final Text e = new Text(value);
            e.setStyle("-fx-text-fill: white");
            e.setFill(Color.WHITE);
            EditorController.this.consoleOut.getChildren().add(e);
            EditorController.this.consoleScroll.setVvalue(1.0);
            return string(value);
        }
    }
}
