#include<pulse/introspect.h>
#include<pulse/context.h>
#include<pulse/error.h>
#include<pulse/mainloop.h>
#include<pulse/subscribe.h>

#include<stdio.h>
#include<stdlib.h>

uint32_t sink_index = 0;

void print_sink_status(const pa_sink_info *i) {
  if(i == NULL) {
    return;
  }
  pa_cvolume volume = i->volume;
  if(volume.channels < 1) {
    return;
  }
  double avg_volume = 0;
  for(uint8_t i = 0; i < volume.channels; i++) {
    avg_volume += ((double) volume.values[i])/ ((double) PA_VOLUME_NORM);
  }
  avg_volume /= volume.channels;

  printf("%.0f%s\n", avg_volume * 100, i->mute ? " mute" : "");
  fflush(stdout);
}

void sink_info_callback(pa_context *c, const pa_sink_info *i, int eol, void *userdata) {
  print_sink_status(i);
}

void request_sink_info(pa_context *c) {
  pa_operation *op = pa_context_get_sink_info_by_index(c, sink_index, &sink_info_callback, NULL);
  if(op == NULL) {
    fprintf(stderr, "could not request sink_info: %s\n", pa_strerror(pa_context_errno(c)));
    exit(-1);
  }
  pa_operation_unref(op);
}


void subscribe_callback(pa_context *c, pa_subscription_event_type_t t,
    uint32_t idx, void *userdata) {

  request_sink_info(c);
}

void subscription_success_callback(pa_context *c, int success, void *userdata) {
  if(!success) {
    fprintf(stderr, "callback subscription not successful: %s\n", pa_strerror(pa_context_errno(c)));
    exit(-1);
  }
}

void subscribe(pa_context *c) {
  pa_operation *op = pa_context_subscribe(c, PA_SUBSCRIPTION_MASK_SINK , &subscription_success_callback, NULL);
  if(op == NULL) {
    fprintf(stderr, "could not subscribe callback : %s\n", pa_strerror(pa_context_errno(c)));
    exit(-1);
  }
  pa_operation_unref(op);
}

void state_callback(pa_context *c, void *userdata) {

  switch(pa_context_get_state(c)) {
    case PA_CONTEXT_READY:
      request_sink_info(c);
      subscribe(c);
      break;
    case PA_CONTEXT_FAILED:
      fprintf(stderr,"The connection failed or was disconnected.\n");
      exit(-1);
      break;
  }

}

int main(int argc, char **argv) {

  if(argc > 1) {
    sink_index = argv[1][0] - '0';
    if(sink_index > 9) {
      sink_index = 0;
    }
  }

  pa_mainloop *mainloop = pa_mainloop_new();

  pa_context *context = pa_context_new(pa_mainloop_get_api(mainloop), "simple-volume-monitor");

  if(context == NULL) {
    fprintf(stderr, "could not create context: %s\n", pa_strerror(pa_context_errno(context)));
    return -1;
  }

  pa_context_set_state_callback(context, &state_callback, NULL);
  
  pa_context_set_subscribe_callback(context, &subscribe_callback, NULL);
  
  if(pa_context_connect(context, NULL, PA_CONTEXT_NOFLAGS, NULL) < 0) {
    fprintf(stderr, "could not connect context: %s\n", pa_strerror(pa_context_errno(context)));
    return -1;
  }

  int retval;

  pa_mainloop_run(mainloop, &retval);

  pa_context_disconnect(context);

  pa_mainloop_free(mainloop);

  return retval;
}
