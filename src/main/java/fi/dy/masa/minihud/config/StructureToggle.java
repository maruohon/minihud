package fi.dy.masa.minihud.config;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.option.BooleanConfig;
import fi.dy.masa.malilib.config.option.ColorConfig;
import fi.dy.masa.malilib.config.option.HotkeyConfig;
import fi.dy.masa.malilib.input.Hotkey;
import fi.dy.masa.minihud.data.DataStorage;

public enum StructureToggle
{
    OVERLAY_STRUCTURE_DESERT_PYRAMID    ("Desert Pyramid",  "", "#30FFFF00", "#30FFFF00", "Toggle Desert Pyramid structure bounding boxes rendering", "Desert Pyramid"),
    OVERLAY_STRUCTURE_END_CITY          ("End City",        "", "#30EB07EB", "#30EB07EB", "Toggle End City structure bounding boxes rendering", "End City"),
    OVERLAY_STRUCTURE_IGLOO             ("Igloo",           "", "#300FAFE4", "#300FAFE4", "Toggle Igloo structure bounding boxes rendering", "Igloo structures"),
    OVERLAY_STRUCTURE_JUNGLE_TEMPLE     ("Jungle Temple",   "", "#3099FF00", "#3099FF00", "Toggle Jungle Temple structure bounding boxes rendering", "Jungle Temple"),
    OVERLAY_STRUCTURE_MANSION           ("Mansion",         "", "#30FF6500", "#30FF6500", "Toggle Mansion structure bounding boxes rendering", "Mansion structures"),
    OVERLAY_STRUCTURE_NETHER_FORTRESS   ("Nether Fortress", "", "#30FC381D", "#30FC381D", "Toggle Nether Fortress structure bounding boxes rendering", "Nether Fortress"),
    OVERLAY_STRUCTURE_OCEAN_MONUMENT    ("Ocean Monument",  "", "#3029E6EF", "#3029E6EF", "Toggle Ocean Monument structure bounding boxes rendering", "Ocean Monument"),
    OVERLAY_STRUCTURE_STRONGHOLD        ("Stronghold",      "", "#30009999", "#30009999", "Toggle Stronghold structure bounding boxes rendering", "Stronghold"),
    OVERLAY_STRUCTURE_VILLAGE           ("Village",         "", "#3054CB4E", "#3054CB4E", "Toggle Village structure bounding boxes rendering\nNOTE: This is the Village world gen structures!\nThis is not the Village data you use for iron farms etc.!", "Village"),
    OVERLAY_STRUCTURE_WITCH_HUT         ("Witch Hut",       "", "#30BE1DFC", "#300099FF", "Toggle Witch Hut structure bounding boxes rendering", "Witch Hut");

    private final BooleanConfig toggleOption;
    private final ColorConfig colorMain;
    private final ColorConfig colorComponents;
    private final Hotkey hotkey;

    StructureToggle(String name, String defaultHotkey, String colorMain, String colorComponents, String comment, String prettyName)
    {
        this.toggleOption    = new BooleanConfig(name, false, comment, prettyName);
        this.colorMain       = new ColorConfig(name +  " Main", colorMain, prettyName + " full box");
        this.colorComponents = new ColorConfig(name + " Components", colorComponents, prettyName + " components");
        this.hotkey          = new HotkeyConfig("Toggle " + name, defaultHotkey, comment);

        this.hotkey.getKeyBind().setCallback((action, key) -> { this.toggleOption.toggleBooleanValue(); return true; });
        this.toggleOption.setValueChangeCallback((newValue, oldValue) -> DataStorage.getInstance().getStructureStorage().requestStructureDataUpdates());
    }

    public BooleanConfig getToggleOption()
    {
        return this.toggleOption;
    }

    public ColorConfig getColorMain()
    {
        return this.colorMain;
    }

    public ColorConfig getColorComponents()
    {
        return this.colorComponents;
    }

    public Hotkey getHotkey()
    {
        return this.hotkey;
    }

    public static ImmutableList<ColorConfig> getColorConfigs()
    {
        ImmutableList.Builder<ColorConfig> builder = ImmutableList.builder();

        for (StructureToggle toggle : values())
        {
            builder.add(toggle.getColorMain());
            builder.add(toggle.getColorComponents());
        }

        return builder.build();
    }

    public static ImmutableList<BooleanConfig> getToggleConfigs()
    {
        ImmutableList.Builder<BooleanConfig> builder = ImmutableList.builder();

        for (StructureToggle toggle : values())
        {
            builder.add(toggle.getToggleOption());
        }

        return builder.build();
    }

    public static ImmutableList<Hotkey> getHotkeys()
    {
        ImmutableList.Builder<Hotkey> builder = ImmutableList.builder();

        for (StructureToggle toggle : values())
        {
            builder.add(toggle.getHotkey());
        }

        return builder.build();
    }
}
