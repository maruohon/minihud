package fi.dy.masa.minihud.config;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.IConfigBoolean;
import fi.dy.masa.malilib.config.options.ConfigBoolean;
import fi.dy.masa.malilib.config.options.ConfigColor;
import fi.dy.masa.malilib.config.options.ConfigHotkey;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.minihud.hotkeys.KeyCallbackToggleStructures;

public enum StructureToggle
{
    OVERLAY_STRUCTURE_DESERT_PYRAMID    ("Desert Pyramid",  "", "#30FFFF00", "#30FFFF00", "Toggle Desert Pyramid structure bounding boxes rendering", "Desert Pyramid"),
    OVERLAY_STRUCTURE_END_CITY          ("End City",        "", "#30EB07EB", "#30EB07EB", "Toggle End City structure bounding boxes rendering", "End City"),
    OVERLAY_STRUCTURE_IGLOO             ("Ingoo",           "", "#300FAFE4", "#300FAFE4", "Toggle Igloo structure bounding boxes rendering", "Igloo structures"),
    OVERLAY_STRUCTURE_JUNGLE_TEMPLE     ("Jungle Temple",   "", "#3099FF00", "#3099FF00", "Toggle Jungle Temple structure bounding boxes rendering", "Jungle Temple"),
    OVERLAY_STRUCTURE_MANSION           ("Mansion",         "", "#30FF6500", "#30FF6500", "Toggle Mansion structure bounding boxes rendering", "Mansion structures"),
    OVERLAY_STRUCTURE_NETHER_FORTRESS   ("Nether Fortress", "", "#30FC381D", "#30FC381D", "Toggle Nether Fortress structure bounding boxes rendering", "Nether Fortress"),
    OVERLAY_STRUCTURE_OCEAN_MONUMENT    ("Ocean Monument",  "", "#3029E6EF", "#3029E6EF", "Toggle Ocean Monument structure bounding boxes rendering", "Ocean Monument"),
    OVERLAY_STRUCTURE_STRONGHOLD        ("Stronghold",      "", "#30009999", "#30009999", "Toggle Stronghold structure bounding boxes rendering", "Stronghold"),
    OVERLAY_STRUCTURE_VILLAGE           ("Village",         "", "#3054CB4E", "#3054CB4E", "Toggle Village structure bounding boxes rendering\nNOTE: This is the Village world gen structures!\nThis is not the Village data you use for iron farms etc.!", "Village"),
    OVERLAY_STRUCTURE_WITCH_HUT         ("Witch Hut",       "", "#30BE1DFC", "#300099FF", "Toggle Witch Hut structure bounding boxes rendering", "Witch Hut");

    private final IConfigBoolean toggleOption;
    private final ConfigColor colorMain;
    private final ConfigColor colorComponents;
    private final IHotkey hotkey;

    private StructureToggle(String name, String defaultHotkey, String colorMain, String colorComponents, String comment, String prettyName)
    {
        this.toggleOption    = new ConfigBoolean(name, false, comment, prettyName);
        this.colorMain       = new ConfigColor(name +  " Main", colorMain, prettyName + " full box");
        this.colorComponents = new ConfigColor(name + " Components", colorComponents, prettyName + " components");
        this.hotkey          = new ConfigHotkey(name, defaultHotkey, comment);
        this.hotkey.getKeybind().setCallback(new KeyCallbackToggleStructures(this.toggleOption));
    }

    public IConfigBoolean getToggleOption()
    {
        return this.toggleOption;
    }

    public ConfigColor getColorMain()
    {
        return this.colorMain;
    }

    public ConfigColor getColorComponents()
    {
        return this.colorComponents;
    }

    public IHotkey getHotkey()
    {
        return this.hotkey;
    }

    public static ImmutableList<ConfigColor> getColorConfigs()
    {
        ImmutableList.Builder<ConfigColor> builder = ImmutableList.builder();

        for (StructureToggle toggle : values())
        {
            builder.add(toggle.getColorMain());
            builder.add(toggle.getColorComponents());
        }

        return builder.build();
    }

    public static ImmutableList<IConfigBoolean> getToggleConfigs()
    {
        ImmutableList.Builder<IConfigBoolean> builder = ImmutableList.builder();

        for (StructureToggle toggle : values())
        {
            builder.add(toggle.getToggleOption());
        }

        return builder.build();
    }

    public static ImmutableList<IHotkey> getHotkeys()
    {
        ImmutableList.Builder<IHotkey> builder = ImmutableList.builder();

        for (StructureToggle toggle : values())
        {
            builder.add(toggle.getHotkey());
        }

        return builder.build();
    }
}
