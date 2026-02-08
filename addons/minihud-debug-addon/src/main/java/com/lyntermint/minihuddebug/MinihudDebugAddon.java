package com.lyntermint.minihuddebug;

import fi.dy.masa.minihud.config.RendererToggle;
import java.util.HashSet;
import java.util.Set;
import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayConnectionEvents;
import net.minecraft.client.MinecraftClient;
import net.minecraft.network.packet.c2s.play.DebugSubscriptionRequestC2SPacket;
import net.minecraft.world.debug.DebugSubscriptionType;
import net.minecraft.world.debug.DebugSubscriptionTypes;

public class MinihudDebugAddon implements ClientModInitializer {
    private Set<DebugSubscriptionType<?>> lastSent;
    private int tickCountdown;

    @Override
    public void onInitializeClient() {
        ClientPlayConnectionEvents.JOIN.register((handler, sender, client) -> {
            this.lastSent = null;
            this.tickCountdown = 0;
            sendIfChanged(client);
        });

        ClientPlayConnectionEvents.DISCONNECT.register((handler, client) -> {
            this.lastSent = null;
        });

        ClientTickEvents.END_CLIENT_TICK.register(client -> {
            if (client == null || client.player == null || client.getNetworkHandler() == null) {
                return;
            }
            if (this.tickCountdown++ < 10) {
                return;
            }
            this.tickCountdown = 0;
            sendIfChanged(client);
        });
    }

    private void sendIfChanged(MinecraftClient client) {
        if (client == null || client.getNetworkHandler() == null) {
            return;
        }

        Set<DebugSubscriptionType<?>> desired = computeDesiredSubscriptions();
        if (this.lastSent != null && this.lastSent.equals(desired)) {
            return;
        }

        this.lastSent = desired;
        client.getNetworkHandler().sendPacket(new DebugSubscriptionRequestC2SPacket(desired));
    }

    private Set<DebugSubscriptionType<?>> computeDesiredSubscriptions() {
        Set<DebugSubscriptionType<?>> types = new HashSet<>();

        if (!RendererToggle.DEBUG_DATA_MAIN_TOGGLE.getBooleanValue()) {
            return types;
        }

        if (RendererToggle.DEBUG_PATH_FINDING.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.ENTITY_PATHS);
        }
        if (RendererToggle.DEBUG_NEIGHBOR_UPDATES.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.NEIGHBOR_UPDATES);
        }
        if (RendererToggle.DEBUG_REDSTONE_UPDATE_ORDER.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.REDSTONE_WIRE_ORIENTATIONS);
        }
        if (RendererToggle.DEBUG_STRUCTURES.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.STRUCTURES);
        }
        if (RendererToggle.DEBUG_VILLAGE_SECTIONS.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.VILLAGE_SECTIONS);
        }
        if (RendererToggle.DEBUG_BRAIN.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.BRAINS);
        }
        if (RendererToggle.DEBUG_POI.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.POIS);
        }
        if (RendererToggle.DEBUG_BEEDATA.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.BEES);
            types.add(DebugSubscriptionTypes.BEE_HIVES);
        }
        if (RendererToggle.DEBUG_RAID_CENTER.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.RAIDS);
        }
        if (RendererToggle.DEBUG_GOAL_SELECTOR.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.GOAL_SELECTORS);
        }
        if (RendererToggle.DEBUG_GAME_EVENT.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.GAME_EVENT_LISTENERS);
            types.add(DebugSubscriptionTypes.GAME_EVENTS);
        }
        if (RendererToggle.DEBUG_BREEZE_JUMP.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.BREEZES);
        }
        if (RendererToggle.DEBUG_ENTITY_BLOCK_INTERSECTION.getBooleanValue()) {
            types.add(DebugSubscriptionTypes.ENTITY_BLOCK_INTERSECTIONS);
        }

        return types;
    }
}
