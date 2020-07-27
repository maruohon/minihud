package fi.dy.masa.minihud.event;

import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.gui.util.GuiUtils;
import fi.dy.masa.malilib.input.IHotkey;
import fi.dy.masa.malilib.input.IKeyBindProvider;
import fi.dy.masa.malilib.input.IMouseInputHandler;
import fi.dy.masa.malilib.input.KeyCallbackAdjustable;
import fi.dy.masa.malilib.input.KeyBindCategory;
import fi.dy.masa.malilib.message.MessageUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;
import fi.dy.masa.minihud.renderer.OverlayRenderer;
import fi.dy.masa.minihud.renderer.OverlayRendererSlimeChunks;

public class InputHandler implements IKeyBindProvider, IMouseInputHandler
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
    public List<? extends IHotkey> getAllHotkeys()
    {
        ImmutableList.Builder<IHotkey> builder = ImmutableList.builder();

        builder.add(InfoToggle.values());
        builder.add(RendererToggle.values());
        builder.addAll(StructureToggle.getHotkeys());
        builder.addAll(Configs.Generic.HOTKEY_LIST);

        return builder.build();
    }

    @Override
    public List<KeyBindCategory> getHotkeyCategoriesForCombinedView()
    {
        return ImmutableList.of(
                new KeyBindCategory(Reference.MOD_NAME, "minihud.hotkeys.category.generic_hotkeys", Configs.Generic.HOTKEY_LIST),
                new KeyBindCategory(Reference.MOD_NAME, "minihud.hotkeys.category.info_toggle_hotkeys", ImmutableList.copyOf(InfoToggle.values())),
                new KeyBindCategory(Reference.MOD_NAME, "minihud.hotkeys.category.renderer_toggle_hotkeys", ImmutableList.copyOf(RendererToggle.values())),
                new KeyBindCategory(Reference.MOD_NAME, "minihud.hotkeys.category.structure_toggle_hotkeys", ImmutableList.copyOf(StructureToggle.getHotkeys()))
        );
    }

    @Override
    public boolean onMouseInput(int eventButton, int wheelDelta, boolean eventButtonState)
    {
        // Not in a GUI
        if (GuiUtils.getCurrentScreen() == null && wheelDelta != 0)
        {
            if (Configs.Generic.CHUNK_UNLOAD_BUCKET_HASH_SIZE.getBooleanValue() &&
                InfoToggle.CHUNK_UNLOAD_ORDER.getKeyBind().isKeyBindHeld())
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

                KeyCallbackAdjustable.setValueChanged();

                String preGreen = GuiBase.TXT_GREEN;
                String rst = GuiBase.TXT_RST;
                Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.setIntegerValue(size);
                size = Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue();
                String strValue = preGreen + size + rst;

                MessageUtils.printActionbarMessage("minihud.message.dropped_chunks_hash_size_set_to", strValue);

                return true;
            }
            else if (RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanValue() &&
                     RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getKeyBind().isKeyBindHeld())
            {
                OverlayRendererSlimeChunks.overlayTopY += (wheelDelta < 0 ? 1 : -1);
                KeyCallbackAdjustable.setValueChanged();
                return true;
            }
            else if (RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getBooleanValue() &&
                     RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getKeyBind().isKeyBindHeld())
           {
               OverlayRenderer.chunkUnloadBucketOverlayY += (wheelDelta < 0 ? 1 : -1);
               KeyCallbackAdjustable.setValueChanged();
               return true;
           }
        }

        return false;
    }
}
