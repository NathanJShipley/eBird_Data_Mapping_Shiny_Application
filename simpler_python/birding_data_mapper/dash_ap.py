import dash
from dash import dcc, html
from dash.dependencies import Input, Output
import plotly.express as px
import pandas as pd
import base64
import io
import webbrowser
from threading import Timer

# Create the Dash app
app = dash.Dash(__name__)

# Initialize the default figure
default_fig = px.scatter_mapbox(
    title="Interactive Map of Birding Locations",
    zoom=3,  # Initial zoom level
    center={"lat": 39.8283, "lon": -98.5795}  # Centered around the US (default)
)

# Update map layout for the default view
default_fig.update_layout(
    mapbox_style="open-street-map",  # Default base map style
    margin={"r": 0, "t": 0, "l": 0, "b": 0}
)

# Define the layout of the app
app.layout = html.Div([

    # Header
    html.H1("Interactive Map of Locations", style={'textAlign': 'left', 'paddingLeft': '20px'}),  # Left-align the title

    # Horizontal container for file upload, slider, and color dropdown
    html.Div([
        # File upload component
        html.Div([
            html.Label("Upload eBird CSV File Here:"),
            dcc.Upload(
                id='upload-data',
                children=html.Button('Upload MyEBirdData.csv'),
                multiple=False
            ),
        ], style={'marginRight': '20px', 'textAlign': 'left'}),  # Add right margin for spacing

        # Slider to adjust marker size
        html.Div([
            html.Label("Adjust Marker Size:"),
            dcc.Slider(
                id='marker-size-slider',
                min=4,
                max=10,
                step=1,
                value=7,  # Default value
                marks={i: str(i) for i in range(4, 11, 1)},
                tooltip={'placement': 'bottom', 'always_visible': True},
            ),
        ], style={'textAlign': 'left', 'width': '500px'}),  # Set the width of the container holding the slider

        # Dropdown to select marker color
        html.Div([
            html.Label("Select Marker Color:"),
            dcc.Dropdown(
                id='marker-color-dropdown',
                options=[
                    {'label': 'Black', 'value': 'black'},
                    {'label': 'Red', 'value': 'red'},
                    {'label': 'White', 'value': 'white'},
                    {'label': 'Green', 'value': 'green'}
                ],
                value='black',  # Default value
                style={'width': '150px'}
            ),
        ], style={'textAlign': 'left', 'marginLeft': '20px'})  # Add left margin for spacing

    ], style={'display': 'flex', 'alignItems': 'center', 'marginBottom': '20px'}),  # Flexbox for horizontal alignment

    # Graph for the map
    dcc.Graph(
        id='graph-geo',
        figure=default_fig,
        config={
            'scrollZoom': True  # Enable zoom with scroll wheel
        },
        style={'width': '90vw', 'height': '80vh'}
    )
])


# Callback to update marker size, color, and data based on user inputs
@app.callback(
    Output('graph-geo', 'figure'),
    [Input('marker-size-slider', 'value'),
     Input('upload-data', 'contents'),
     Input('graph-geo', 'relayoutData'),  # Capture the current map's layout state
     Input('marker-color-dropdown', 'value')]  # Capture the selected marker color
)
def update_marker_size_and_data(marker_size, uploaded_file, relayout_data, marker_color):
    # Start with the default figure
    updated_fig = default_fig

    # Default zoom and center values
    current_zoom = 3  # Default zoom level
    current_center = {"lat": 39.8283, "lon": -98.5795}  # Default center (US)

    # Check if relayout_data is not None and contains the map layout
    if relayout_data:
        # Safely retrieve zoom and center if they exist
        current_zoom = relayout_data.get('mapbox.zoom', current_zoom)
        current_center = relayout_data.get('mapbox.center', current_center)

    # Check if a file was uploaded
    if uploaded_file is not None:
        content_type, content_string = uploaded_file.split(',')
        decoded = base64.b64decode(content_string)
        try:
            # Read the CSV content into a pandas dataframe
            df = pd.read_csv(io.StringIO(decoded.decode('utf-8')))

            # Check if 'Latitude' and 'Longitude' columns exist
            if 'Latitude' in df.columns and 'Longitude' in df.columns:
                # Filter the necessary columns for latitude and longitude, then drop duplicates
                df_filtered = df[['Latitude', 'Longitude']].drop_duplicates()

                # Check if there are any valid coordinates in the filtered data
                if df_filtered.empty:
                    return updated_fig  # Return the default empty map if no valid data

                # Create a map with the filtered data
                updated_fig = px.scatter_mapbox(df_filtered,
                                                lat='Latitude',
                                                lon='Longitude',
                                                zoom=current_zoom,  # Keep the current zoom level
                                                center=current_center)  # Keep the current center
                updated_fig.update_layout(
                    mapbox_style="open-street-map",
                )
            else:
                return updated_fig  # Return the default map if lat/lon are missing
        except Exception as e:
            print(f"Error processing file: {e}")
            return updated_fig  # Return default map if there's an issue processing the file

    # Update the map's marker size, color, and preserve the zoom and center
    updated_fig.update_traces(marker=dict(
        size=marker_size,
        color=marker_color  # Set the selected marker color
    ))

    # Update the figure's layout to keep the current zoom and center
    updated_fig.update_layout(
        mapbox=dict(
            zoom=current_zoom,
            center=current_center
        )
    )

    return updated_fig


# Function to open the browser after a delay
def open_browser():
    webbrowser.open("http://127.0.0.1:8050/")  # URL Dash app is hosted on


# Run the app and automatically open in browser
if __name__ == '__main__':
    # Set a timer to open the browser
    Timer(1, open_browser).start()  # Wait 1 second before opening the browser
    app.run_server(debug=True, use_reloader=False)  # Ensure app doesn't reload automatically
