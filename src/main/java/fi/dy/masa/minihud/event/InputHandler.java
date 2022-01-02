package fi.dy.masa.minihud.event;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.malilib.hotkeys.IKeybindManager;
import fi.dy.masa.malilib.hotkeys.IKeybindProvider;
import fi.dy.masa.malilib.hotkeys.IMouseInputHandler;
import fi.dy.masa.malilib.hotkeys.KeyCallbackAdjustable;
import fi.dy.masa.malilib.util.GuiUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.renderer.OverlayRendererSlimeChunks;

public class InputHandler implements IKeybindProvider, IMouseInputHandler
{
    private static final InputHandler INSTANCE = new InputHandler();

    private InputHandler()
    {
        super();
    }

    public static InputHandler getInstance()
    {
        return INSTANCE;
    }

    @Override
    public void addKeysToMap(IKeybindManager manager)
    {
        for (InfoToggle toggle : InfoToggle.VALUES)
        {
            manager.addKeybindToMap(toggle.getKeybind());
        }

        for (RendererToggle toggle : RendererToggle.VALUES)
        {
            manager.addKeybindToMap(toggle.getKeybind());
        }

        for (IHotkey hotkey : Configs.Generic.HOTKEY_LIST)
        {
            manager.addKeybindToMap(hotkey.getKeybind());
        }
    }

    @Override
    public void addHotkeys(IKeybindManager manager)
    {
        manager.addHotkeysForCategory(Reference.MOD_NAME, "minihud.hotkeys.category.generic_hotkeys", Configs.Generic.HOTKEY_LIST);
        manager.addHotkeysForCategory(Reference.MOD_NAME, "minihud.hotkeys.category.info_toggle_hotkeys", ImmutableList.copyOf(InfoToggle.VALUES));
        manager.addHotkeysForCategory(Reference.MOD_NAME, "minihud.hotkeys.category.renderer_toggle_hotkeys", ImmutableList.copyOf(RendererToggle.VALUES));
    }

    @Override
    public boolean onMouseScroll(int mouseX, int mouseY, double dWheel)
    {
        // Not in a GUI
        if (GuiUtils.getCurrentScreen() == null && dWheel != 0)
        {
            if (RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanValue() &&
                RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getKeybind().isKeybindHeld())
            {
                OverlayRendererSlimeChunks.overlayTopY += (dWheel < 0 ? 1 : -1);
                KeyCallbackAdjustable.setValueChanged();
                return true;
            }
        }

        return false;
    }
}
