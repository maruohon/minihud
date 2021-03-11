package fi.dy.masa.minihud.event;

import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.gui.util.GuiUtils;
import fi.dy.masa.malilib.input.Hotkey;
import fi.dy.masa.malilib.input.KeyBindCategory;
import fi.dy.masa.malilib.input.KeyBindProvider;
import fi.dy.masa.malilib.input.MouseInputHandler;
import fi.dy.masa.malilib.input.callback.AdjustableKeyCallback;
import fi.dy.masa.malilib.message.MessageUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoLine;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;
import fi.dy.masa.minihud.renderer.OverlayRenderer;
import fi.dy.masa.minihud.renderer.OverlayRendererSlimeChunks;

public class InputHandler implements KeyBindProvider, MouseInputHandler
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
    public List<? extends Hotkey> getAllHotkeys()
    {
        ImmutableList.Builder<Hotkey> builder = ImmutableList.builder();

        builder.addAll(InfoLine.TOGGLE_HOTKEYS);
        builder.addAll(RendererToggle.TOGGLE_HOTKEYS);
        builder.addAll(StructureToggle.TOGGLE_HOTKEYS);
        builder.addAll(Configs.Generic.HOTKEY_LIST);

        return builder.build();
    }

    @Override
    public List<KeyBindCategory> getHotkeyCategoriesForCombinedView()
    {
        return ImmutableList.of(
                new KeyBindCategory(Reference.MOD_ID, Reference.MOD_NAME, "minihud.hotkeys.category.generic_hotkeys", Configs.Generic.HOTKEY_LIST),
                new KeyBindCategory(Reference.MOD_ID, Reference.MOD_NAME, "minihud.hotkeys.category.info_toggle_hotkeys", InfoLine.TOGGLE_HOTKEYS),
                new KeyBindCategory(Reference.MOD_ID, Reference.MOD_NAME, "minihud.hotkeys.category.renderer_toggle_hotkeys", RendererToggle.TOGGLE_HOTKEYS),
                new KeyBindCategory(Reference.MOD_ID, Reference.MOD_NAME, "minihud.hotkeys.category.structure_toggle_hotkeys", StructureToggle.TOGGLE_HOTKEYS)
        );
    }

    @Override
    public boolean onMouseInput(int eventButton, int wheelDelta, boolean eventButtonState)
    {
        // Not in a GUI
        if (GuiUtils.getCurrentScreen() == null && wheelDelta != 0)
        {
            if (Configs.Generic.CHUNK_UNLOAD_BUCKET_HASH_SIZE.getBooleanValue() &&
                InfoLine.CHUNK_UNLOAD_ORDER.getHotkeyConfig().getKeyBind().isKeyBindHeld())
            {
                int size = Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue();

                if (size == 0)
                {
                    size = wheelDelta < 0 ? 1 : -1;
                }
                else if (wheelDelta < 0)
                {
                    size <<= 1;
                }
                else
                {
                    size >>>= 1;
                }

                AdjustableKeyCallback.setValueChanged();

                String preGreen = BaseScreen.TXT_GREEN;
                String rst = BaseScreen.TXT_RST;
                Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.setValue(size);
                size = Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue();
                String strValue = preGreen + size + rst;

                MessageUtils.printActionbarMessage("minihud.message.dropped_chunks_hash_size_set_to", strValue);

                return true;
            }
            else if (RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.isRendererEnabled() &&
                     RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getHotkeyConfig().getKeyBind().isKeyBindHeld())
            {
                OverlayRendererSlimeChunks.overlayTopY += (wheelDelta < 0 ? 1 : -1);
                AdjustableKeyCallback.setValueChanged();
                return true;
            }
            else if (RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.isRendererEnabled() &&
                     RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getHotkeyConfig().getKeyBind().isKeyBindHeld())
           {
               OverlayRenderer.chunkUnloadBucketOverlayY += (wheelDelta < 0 ? 1 : -1);
               AdjustableKeyCallback.setValueChanged();
               return true;
           }
        }

        return false;
    }
}
