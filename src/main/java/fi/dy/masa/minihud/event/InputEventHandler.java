package fi.dy.masa.minihud.event;

import fi.dy.masa.malilib.hotkeys.IKeybindEventHandler;
import fi.dy.masa.malilib.hotkeys.IKeybindManager;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;

public class InputEventHandler implements IKeybindEventHandler
{
    private static final InputEventHandler INSTANCE = new InputEventHandler();

    private InputEventHandler()
    {
        super();
    }

    public static InputEventHandler getInstance()
    {
        return INSTANCE;
    }

    @Override
    public void addKeysToMap(IKeybindManager manager)
    {
        for (InfoToggle toggle : InfoToggle.values())
        {
            manager.addKeybindToMap(toggle.getKeybind());
        }

        for (RendererToggle toggle : RendererToggle.values())
        {
            manager.addKeybindToMap(toggle.getKeybind());
        }

        manager.addKeybindToMap(Configs.Generic.TOGGLE_KEY.getKeybind());
        manager.addKeybindToMap(Configs.Generic.REQUIRED_KEY.getKeybind());
    }

    @Override
    public boolean onKeyInput(int eventKey, boolean eventKeyState)
    {
        return false;
    }

    @Override
    public boolean onMouseInput(int eventButton, int dWheel, boolean eventButtonState)
    {
        return false;
    }
}
