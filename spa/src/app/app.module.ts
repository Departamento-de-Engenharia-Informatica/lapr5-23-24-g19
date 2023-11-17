import { NgModule, isDevMode } from '@angular/core';
import { HttpClientModule } from '@angular/common/http';
import { BrowserModule } from '@angular/platform-browser';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { BuildingComponent } from './components/building/building.component';
import { ElevatorComponent } from './components/elevator/elevator.component';
import { FloorComponent } from './components/floor/floor.component';
import { RoomComponent } from './components/room/room.component';
import { RobotComponent } from './components/robot/robot.component';
import { RobotTypeComponent } from './components/robot-type/robot-type.component';
import { PassageComponent } from './components/passage/passage.component';
import { PageNotFoundComponent } from './components/page-not-found/page-not-found.component';
import { Visualization3DComponent } from './components/visualization3-d/visualization3-d.component';
import { CampusComponent } from './components/campus-menu/campus-menu.component';
import { RouterModule } from '@angular/router';
import { ListFloorsComponent } from './components/floor/list-floors/list-floors.component';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { GetBuildingsComponent } from './components/get-buildings/get-buildings.component';
import { ContactInfoComponent } from './components/contact-info/contact-info.component';
import { ModulesComponent } from './components/modules/modules.component';
import { BuildingService } from './services/building.service';
import { EditBuildingComponent } from './components/edit-building/edit-building.component';
import { ElevatorService } from './services/elevator.service';
import { CreateElevatorComponent } from './components/elevator/create-elevator/create-elevator.component';
import { ListElevatorsComponent } from './components/elevator/list-elevators/list-elevators.component';
import { CreateFloorComponent } from './components/floor/create-floor/create-floor.component';
import { CreateBuildingComponent } from './components/create-building/create-building.component';
import { PassageService } from './services/passage.service';
import { ListPassagesBetweenBuildingsComponent } from './components/passage/list-passages-between-buildings/list-passages-between-buildings.component';
import { PatchFloorComponent } from './components/floor/patch-floor/patch-floor.component';
import { ListBuildingsMinmaxFloorsComponent } from './components/building/list-buildings-minmax-floors/list-buildings-minmax-floors.component';
import { TaskMenuComponent } from './components/task-menu/task-menu.component';
import { FleetMenuComponent } from './components/fleet-menu/fleet-menu.component';
import { environment } from 'src/environment/environment';
import { environment as prod } from 'src/environment/environment.prod';
import { PutFloorComponent } from './components/floor/put-floor/put-floor.component';
import { ListRobotsComponent } from './components/robot/list-robots/list-robots.component';
import { RobotService } from './services/robot.service';

@NgModule({
    declarations: [
        AppComponent,
        BuildingComponent,
        ElevatorComponent,
        FloorComponent,
        RoomComponent,
        RobotComponent,
        RobotTypeComponent,
        PassageComponent,
        PageNotFoundComponent,
        Visualization3DComponent,
        CampusComponent,
        ListFloorsComponent,
        ModulesComponent,
        ContactInfoComponent,
        GetBuildingsComponent,
        EditBuildingComponent,
        CreateElevatorComponent,
        ListElevatorsComponent,
        CreateFloorComponent,
        CreateBuildingComponent,
        ListPassagesBetweenBuildingsComponent,
        PatchFloorComponent,
        ListBuildingsMinmaxFloorsComponent,
        TaskMenuComponent,
        FleetMenuComponent,
        PutFloorComponent,
        ListRobotsComponent,
    ],
    imports: [
        BrowserModule,
        FormsModule,
        AppRoutingModule,
        HttpClientModule,
        RouterModule,
        FormsModule,
        ReactiveFormsModule,
    ],
    providers: [BuildingService, ElevatorService, PassageService, RobotService],
    bootstrap: [AppComponent],
})
export class AppModule {
    public static baseUrl: string;
    public static visualizationUrl: string;
    constructor() {
        if (isDevMode()) {
            AppModule.baseUrl = environment.mdrUrl;
            AppModule.visualizationUrl = environment.mdvUrl;
        } else {
            AppModule.baseUrl = prod.mdrUrl;
            AppModule.visualizationUrl = prod.mdvUrl;
        }
    }
}
