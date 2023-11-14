import { NgModule } from '@angular/core';
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
import { MainMenuComponent } from './components/main-menu/main-menu.component';
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
        MainMenuComponent,
        ListFloorsComponent,
        ModulesComponent,
        ContactInfoComponent,
        GetBuildingsComponent,
        EditBuildingComponent,
        CreateElevatorComponent,
        ListElevatorsComponent,
        CreateFloorComponent,
        CreateBuildingComponent
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
    providers: [BuildingService, ElevatorService],
    bootstrap: [AppComponent],
})
export class AppModule {
    public static baseUrl = 'http://localhost:4000/api';
}
