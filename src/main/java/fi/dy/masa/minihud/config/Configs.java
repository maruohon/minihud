package fi.dy.masa.minihud.config;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.lwjgl.input.Keyboard;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.mumfrey.liteloader.core.LiteLoader;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.event.InputEventHandler.KeyModifier;
import fi.dy.masa.minihud.event.RenderEventHandler;
import fi.dy.masa.minihud.util.JsonUtils;

public class Configs
{
    private static final String CONFIG_FILE_NAME = Reference.MOD_ID + ".json";

    public static KeyModifier requiredKey;
    private static final Multimap<Integer, Integer> HOTKEY_DEBUG_MAP = HashMultimap.create();
    private static final Multimap<Integer, Integer> HOTKEY_INFO_MAP = HashMultimap.create();
    private static final Map<Integer, Integer> LINE_ORDER_MAP = new HashMap<Integer, Integer>();

    public enum ConfigType
    {
        BOOLEAN,
        INTEGER,
        STRING,
        HEX_STRING,
        HOTKEY;
    }

    public static void load()
    {
        File configFile = new File(LiteLoader.getCommonConfigFolder(), CONFIG_FILE_NAME);

        if (configFile.exists() && configFile.isFile() && configFile.canRead())
        {
            JsonElement element = JsonUtils.parseJsonFile(configFile);

            if (element != null && element.isJsonObject())
            {
                JsonObject root = element.getAsJsonObject();
                JsonObject objToggles           = JsonUtils.getNestedObject(root, "InfoTypeToggles", false);
                JsonObject objInfoHotkeys       = JsonUtils.getNestedObject(root, "InfoTypeHotkeys", false);
                JsonObject objInfoLineOrders    = JsonUtils.getNestedObject(root, "InfoLineOrders", false);
                JsonObject objGeneric           = JsonUtils.getNestedObject(root, "Generic", false);
                JsonObject objDebugHotkeys      = JsonUtils.getNestedObject(root, "DebugRendererHotkeys", false);

                if (objGeneric != null)
                {
                    for (ConfigsGeneric gen : ConfigsGeneric.values())
                    {
                        if (objGeneric.has(gen.getName()) && objGeneric.get(gen.getName()).isJsonPrimitive())
                        {
                            gen.setValueFromJsonPrimitive(objGeneric.get(gen.getName()).getAsJsonPrimitive());
                        }
                    }
                }

                for (InfoToggle toggle : InfoToggle.values())
                {
                    if (objToggles != null && JsonUtils.hasBoolean(objToggles, toggle.getName()))
                    {
                        toggle.setBooleanValue(JsonUtils.getBoolean(objToggles, toggle.getName()));
                    }

                    if (objInfoHotkeys != null && JsonUtils.hasString(objInfoHotkeys, toggle.getName()))
                    {
                        toggle.setHotkey(JsonUtils.getString(objInfoHotkeys, toggle.getName()));
                    }

                    if (objInfoLineOrders != null && JsonUtils.hasInteger(objInfoLineOrders, toggle.getName()))
                    {
                        toggle.setLinePosition(JsonUtils.getInteger(objInfoLineOrders, toggle.getName()));
                    }
                }

                if (objDebugHotkeys != null)
                {
                    for (DebugHotkeys dbg : DebugHotkeys.values())
                    {
                        if (JsonUtils.hasString(objDebugHotkeys, dbg.getName()))
                        {
                            dbg.setHotkey(JsonUtils.getString(objDebugHotkeys, dbg.getName()));
                        }
                    }
                }
            }
        }

        requiredKey = getKeyModifier(ConfigsGeneric.REQUIRE_HOLDING_KEY.getStringValue());
        int enabledInfoTypes = 0;
        HOTKEY_INFO_MAP.clear();

        for (InfoToggle toggle : InfoToggle.values())
        {
            enabledInfoTypes = toggle.applyBitMask(enabledInfoTypes);
            assignHotkey(HOTKEY_INFO_MAP, toggle);
            LINE_ORDER_MAP.put(toggle.getBitMask(), toggle.getLinePosition());
        }

        HOTKEY_DEBUG_MAP.clear();

        for (DebugHotkeys dbg : DebugHotkeys.values())
        {
            assignHotkey(HOTKEY_DEBUG_MAP, dbg.getHotkey(), dbg.getBitMask());
        }

        RenderEventHandler.getInstance().setEnabledMask(enabledInfoTypes);
    }

    public static void save()
    {
        File dir = LiteLoader.getCommonConfigFolder();

        if (dir.exists() && dir.isDirectory())
        {
            File configFile = new File(dir, CONFIG_FILE_NAME);
            FileWriter writer = null;
            JsonObject root = new JsonObject();
            JsonObject objToggles           = JsonUtils.getNestedObject(root, "InfoTypeToggles", true);
            JsonObject objInfoHotkeys       = JsonUtils.getNestedObject(root, "InfoTypeHotkeys", true);
            JsonObject objInfoLineOrders    = JsonUtils.getNestedObject(root, "InfoLineOrders", true);
            JsonObject objGeneric           = JsonUtils.getNestedObject(root, "Generic", true);
            JsonObject objDebugHotkeys      = JsonUtils.getNestedObject(root, "DebugRendererHotkeys", true);

            for (ConfigsGeneric gen : ConfigsGeneric.values())
            {
                objGeneric.add(gen.getName(), gen.getAsJsonPrimitive());
            }

            for (InfoToggle toggle : InfoToggle.values())
            {
                objToggles.add(toggle.getName(), new JsonPrimitive(toggle.getBooleanValue()));
                objInfoHotkeys.add(toggle.getName(), new JsonPrimitive(toggle.getHotkey()));
                objInfoLineOrders.add(toggle.getName(), new JsonPrimitive(toggle.getLinePosition()));
            }

            for (DebugHotkeys dbg : DebugHotkeys.values())
            {
                objDebugHotkeys.add(dbg.getName(), new JsonPrimitive(dbg.getHotkey()));
            }

            try
            {
                writer = new FileWriter(configFile);
                writer.write(JsonUtils.GSON.toJson(root));
                writer.close();
            }
            catch (IOException e)
            {
                LiteModMiniHud.logger.warn("Failed to write configs to file '{}'", configFile.getAbsolutePath(), e);
            }
            finally
            {
                try
                {
                    if (writer != null)
                    {
                        writer.close();
                    }
                }
                catch (Exception e)
                {
                    LiteModMiniHud.logger.warn("Failed to close config file", e);
                }
            }
        }
    }

    private static KeyModifier getKeyModifier(String value)
    {
        if (value == null)
        {
            return KeyModifier.NONE;
        }

        if (value.equalsIgnoreCase("shift"))
        {
            return KeyModifier.SHIFT;
        }

        if (value.equalsIgnoreCase("ctrl") || value.equalsIgnoreCase("control"))
        {
            return KeyModifier.CONTROL;
        }

        if (value.equalsIgnoreCase("alt"))
        {
            return KeyModifier.ALT;
        }

        return KeyModifier.NONE;
    }

    private static void assignHotkey(Multimap<Integer, Integer> map, InfoToggle toggle)
    {
        assignHotkey(map, toggle.getHotkey(), toggle.getBitMask());
    }

    private static void assignHotkey(Multimap<Integer, Integer> map, String keyName, int bitmask)
    {
        try
        {
            int keyCode = Integer.parseInt(keyName);

            // Don't interpret the numbers 0..9 as raw keycodes, but instead as the number keys (below)
            if (keyCode > Keyboard.KEY_0)
            {
                map.put(keyCode, bitmask);
                return;
            }
        }
        catch (NumberFormatException e)
        {
        }

        int keyCode = Keyboard.getKeyIndex(keyName.toUpperCase());

        if (keyCode != Keyboard.KEY_NONE)
        {
            map.put(keyCode, bitmask);
        }
    }

    public static int getBitmaskForInfoKey(int keyCode)
    {
        return getBitmaskForKey(HOTKEY_INFO_MAP, keyCode);
    }

    public static int getBitmaskForDebugKey(int keyCode)
    {
        return getBitmaskForKey(HOTKEY_DEBUG_MAP, keyCode);
    }

    private static int getBitmaskForKey(Multimap<Integer, Integer> map, int keyCode)
    {
        Collection<Integer> masks = map.get(keyCode);
        int fullMask = 0;

        if (masks != null)
        {
            for (Integer mask : masks)
            {
                fullMask |= mask;
            }
        }

        return fullMask;
    }

    public static int getLinePositionFor(int infoType)
    {
        Integer value = LINE_ORDER_MAP.get(infoType);
        return value != null ? value.intValue() : -1;
    }
}
