package fi.dy.masa.minihud.event;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.malilib.hotkeys.IKeybindManager;
import fi.dy.masa.malilib.hotkeys.IKeybindProvider;
import fi.dy.masa.malilib.hotkeys.IMouseInputHandler;
import fi.dy.masa.malilib.hotkeys.KeyCallbackAdjustable;
import fi.dy.masa.malilib.util.InfoUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.renderer.OverlayRenderer;
import fi.dy.masa.minihud.renderer.OverlayRendererSlimeChunks;
import fi.dy.masa.minihud.util.MiscUtils;
import net.minecraft.client.Minecraft;

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
        for (InfoToggle toggle : InfoToggle.values())
        {
            manager.addKeybindToMap(toggle.getKeybind());
        }

        for (RendererToggle toggle : RendererToggle.values())
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
        manager.addHotkeysForCategory(Reference.MOD_NAME, "minihud.hotkeys.category.info_toggle_hotkeys", ImmutableList.copyOf(InfoToggle.values()));
        manager.addHotkeysForCategory(Reference.MOD_NAME, "minihud.hotkeys.category.renderer_toggle_hotkeys", ImmutableList.copyOf(RendererToggle.values()));
    }

    @Override
    public boolean onMouseInput(int eventButton, int dWheel, boolean eventButtonState)
    {
        Minecraft mc = Minecraft.getMinecraft();

        // Not in a GUI
        if (mc.currentScreen == null && dWheel != 0)
        {
            if (Configs.Generic.CHUNK_UNLOAD_BUCKET_WITH_SIZE.getBooleanValue() &&
                InfoToggle.CHUNK_UNLOAD_ORDER.getKeybind().isKeybindHeld())
            {
                int size = MiscUtils.getDroppedChunksHashSize();

                if (dWheel < 0)
                {
                    size <<= 1;
                }
                else
                {
                    size >>>= 1;
                }

                KeyCallbackAdjustable.setValueChanged();

                String preGreen = GuiBase.TXT_GREEN;
                String rst = GuiBase.TXT_RST;
                Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.setIntegerValue(size);
                String strValue = preGreen + size + rst;

                InfoUtils.printActionbarMessage("minihud.message.dropped_chunks_hash_size_set_to", strValue);

                return true;
            }
            else if (RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanValue() &&
                     RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getKeybind().isKeybindHeld())
            {
                OverlayRendererSlimeChunks.overlayTopY += (dWheel < 0 ? 1 : -1);
                KeyCallbackAdjustable.setValueChanged();
                return true;
            }
            else if (RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getBooleanValue() &&
                     RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getKeybind().isKeybindHeld())
           {
               OverlayRenderer.chunkUnloadBucketOverlayY += (dWheel < 0 ? 1 : -1);
               KeyCallbackAdjustable.setValueChanged();
               return true;
           }
        }

        return false;
    }
}
