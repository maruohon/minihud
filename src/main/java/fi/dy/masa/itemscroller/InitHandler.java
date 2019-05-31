package fi.dy.masa.itemscroller;

import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.event.InputHandler;
import fi.dy.masa.itemscroller.event.KeybindCallbacks;
import fi.dy.masa.itemscroller.event.WorldLoadListener;
import fi.dy.masa.malilib.config.ConfigManager;
import fi.dy.masa.malilib.event.InputEventHandler;
import fi.dy.masa.malilib.event.WorldLoadHandler;
import fi.dy.masa.malilib.interfaces.IInitializationHandler;

public class InitHandler implements IInitializationHandler
{
    @Override
    public void registerModHandlers()
    {
        ConfigManager.getInstance().registerConfigHandler(Reference.MOD_ID, new Configs());

        InputHandler handler = new InputHandler();
        InputEventHandler.getKeybindManager().registerKeybindProvider(handler);
        InputEventHandler.getInputManager().registerKeyboardInputHandler(handler);
        InputEventHandler.getInputManager().registerMouseInputHandler(handler);

        WorldLoadListener listener = new WorldLoadListener();
        WorldLoadHandler.getInstance().registerWorldLoadPreHandler(listener);
        WorldLoadHandler.getInstance().registerWorldLoadPostHandler(listener);

        KeybindCallbacks.getInstance().setCallbacks();
    }
}
