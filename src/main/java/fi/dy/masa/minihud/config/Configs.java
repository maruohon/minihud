package fi.dy.masa.minihud.config;

import java.io.File;

import net.minecraftforge.common.config.Configuration;
import net.minecraftforge.common.config.Property;
import net.minecraftforge.fml.client.event.ConfigChangedEvent.OnConfigChangedEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.event.RenderEventHandler;

public class Configs
{
    public static boolean useFontShadow;
    public static boolean useScaledFont;
    public static boolean useTextBackground;

    public static int defaultMode;
    public static int fontColor;
    public static int textBackgroundColor;

    public static File configurationFile;
    public static Configuration config;
    
    public static final String CATEGORY_GENERIC = "Generic";

    @SubscribeEvent
    public void onConfigChangedEvent(OnConfigChangedEvent event)
    {
        if (Reference.MOD_ID.equals(event.modID) == true)
        {
            loadConfigs(config);
        }
    }

    public static void loadConfigsFromFile(File configFile)
    {
        configurationFile = configFile;
        config = new Configuration(configFile, "0.1.0", true);
        config.load();

        loadConfigs(config);
    }

    public static void loadConfigs(Configuration conf)
    {
        Property prop;

        prop = conf.get(CATEGORY_GENERIC, "useFontShadow", false);
        prop.comment = "Use font shadow";
        useFontShadow = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "useScaledFont", true);
        prop.comment = "Use 0.5x scale font size";
        useScaledFont = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "useTextBackground", true);
        prop.comment = "Use a solid background color behind the text";
        useTextBackground = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "defaultMode", 1);
        prop.comment = "Bit mask of the enabled information. 1 = coordinates, 2 = yaw, 4 = pitch, 8 = speed, 16 = biome, 32 = light (sum together the ones you want enabled by default)";
        defaultMode = prop.getInt();
        RenderEventHandler.mask = defaultMode;

        prop = conf.get(CATEGORY_GENERIC, "fontColor", 0xE0E0E0);
        prop.comment = "Font color (default = 0xE0E0E0 = 14737632)";
        fontColor = prop.getInt();

        prop = conf.get(CATEGORY_GENERIC, "textBackgroundColor", 0x90505050);
        prop.comment = "Text background color (default = 0x90505050 = -1873784752)";
        textBackgroundColor = prop.getInt();

        if (conf.hasChanged() == true)
        {
            conf.save();
        }
    }
}
