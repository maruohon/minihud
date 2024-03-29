package minihud.input;

import java.util.List;
import com.google.common.collect.ImmutableList;

import malilib.input.Hotkey;
import malilib.input.HotkeyCategory;
import malilib.input.HotkeyProvider;
import minihud.Reference;
import minihud.config.Configs;
import minihud.config.InfoLineToggle;
import minihud.config.RendererToggle;
import minihud.config.StructureToggle;

public class MiniHudHotkeyProvider implements HotkeyProvider
{
    public static final MiniHudHotkeyProvider INSTANCE = new MiniHudHotkeyProvider();

    @Override
    public List<? extends Hotkey> getAllHotkeys()
    {
        ImmutableList.Builder<Hotkey> builder = ImmutableList.builder();

        builder.addAll(InfoLineToggle.TOGGLE_HOTKEYS);
        builder.addAll(RendererToggle.TOGGLE_HOTKEYS);
        builder.addAll(StructureToggle.TOGGLE_HOTKEYS);
        builder.addAll(Configs.Generic.HOTKEY_LIST);
        builder.addAll(Configs.Hotkeys.HOTKEYS);

        return builder.build();
    }

    @Override
    public List<HotkeyCategory> getHotkeysByCategories()
    {
        return ImmutableList.of(
                new HotkeyCategory(Reference.MOD_INFO, "minihud.hotkeys.category.generic", Configs.Generic.HOTKEY_LIST),
                new HotkeyCategory(Reference.MOD_INFO, "minihud.hotkeys.category.hotkeys", Configs.Hotkeys.HOTKEYS),
                new HotkeyCategory(Reference.MOD_INFO, "minihud.hotkeys.category.info_toggle_hotkeys", InfoLineToggle.TOGGLE_HOTKEYS),
                new HotkeyCategory(Reference.MOD_INFO, "minihud.hotkeys.category.renderer_toggle_hotkeys", RendererToggle.TOGGLE_HOTKEYS),
                new HotkeyCategory(Reference.MOD_INFO, "minihud.hotkeys.category.structure_toggle_hotkeys", StructureToggle.TOGGLE_HOTKEYS)
        );
    }
}
