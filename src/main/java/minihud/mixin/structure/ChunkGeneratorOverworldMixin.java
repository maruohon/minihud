package minihud.mixin.structure;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

import net.minecraft.world.gen.ChunkGeneratorOverworld;
import net.minecraft.world.gen.structure.MapGenScatteredFeature;
import net.minecraft.world.gen.structure.MapGenStronghold;
import net.minecraft.world.gen.structure.MapGenVillage;
import net.minecraft.world.gen.structure.StructureOceanMonument;
import net.minecraft.world.gen.structure.WoodlandMansion;

@Mixin(ChunkGeneratorOverworld.class)
public interface ChunkGeneratorOverworldMixin
{
    @Accessor("oceanMonumentGenerator")
    StructureOceanMonument minihud_getOceanMonumentGenerator();

    @Accessor("scatteredFeatureGenerator")
    MapGenScatteredFeature minihud_getScatteredFeatureGenerator();

    @Accessor("strongholdGenerator")
    MapGenStronghold minihud_getStrongholdGenerator();

    @Accessor("villageGenerator")
    MapGenVillage minihud_getVillageGenerator();

    @Accessor("woodlandMansionGenerator")
    WoodlandMansion minihud_getWoodlandMansionGenerator();
}
